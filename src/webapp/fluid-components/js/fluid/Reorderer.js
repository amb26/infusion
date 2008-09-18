/*
Copyright 2007 - 2008 University of Toronto
Copyright 2007 University of Cambridge

Licensed under the Educational Community License (ECL), Version 2.0 or the New
BSD license. You may not use this file except in compliance with one these
Licenses.

You may obtain a copy of the ECL 2.0 License and BSD License at
https://source.fluidproject.org/svn/LICENSE.txt
*/

// Declare dependencies.
/*global jQuery*/
/*global fluid*/

fluid = fluid || {};

(function (jQuery, fluid) {
    
    var defaultAvatarCreator = function(item, cssClass, dropWarning) {
        var avatar = jQuery(item).clone();
        fluid.iterateDom(avatar.get(0), function(node) {
            if (node.tagName.toLowerCase() === "script") {
                return true;
            }
            node.removeAttribute("id");
            if (node.tagName.toLowerCase() === "input") {
                node.setAttribute("disabled", "disabled");
                }
            }
          );
        avatar.removeAttr("id");
        //jQuery("[id]", avatar).removeAttr("id");
        //jQuery(":hidden", avatar).remove();
        //jQuery("input", avatar).attr("disabled", "true");
        // dropping in the same column fails if the avatar is considered a droppable.
        // droppable ("destroy") should take care of this, but it doesn't seem to remove
        // the class, which is what is checked, so we remove it manually
        // (see http://dev.jquery.com/ticket/2599)
        // 2008-05-12: 2599 has been fixed now in trunk
        //                    avatar.droppable ("destroy");
        avatar.removeClass("ui-droppable");
        avatar.addClass(cssClass);
        
        if (dropWarning) {
            // Will a 'div' always be valid in this position?
            var avatarContainer = jQuery(document.createElement("div"));
            avatarContainer.append(avatar);
            avatarContainer.append(dropWarning);
            avatar = avatarContainer;
        }
        jQuery("body").append(avatar);
        avatar.css("display", "block").width(item.offsetWidth).height(item.offsetHeight);
        
        if (jQuery.browser.opera) { // FLUID-1490. Without this detect, curCSS explodes on the avatar on Firefox.
            avatar.hide();
        }
        return avatar;
    };   
    
    fluid.defaults("fluid.reorderer", {
        instructionMessageId: "message-bundle:",
        styles: {
            defaultStyle: "orderable-default",
            selected: "orderable-selected",
            dragging: "orderable-dragging",
            mouseDrag: "orderable-dragging",
            hover: "orderable-hover",
            dropMarker: "orderable-drop-marker",
            avatar: "orderable-avatar"
            },
        selectors: {
            dropWarning: ".drop-warning",
            movables: ".movables",
            grabHandle: "",
            stylisticOffset: ""
        },
        avatarCreator: defaultAvatarCreator,
        keysets: fluid.defaultKeysets,
        layoutHandler: "fluid.listLayoutHandler",
        
        events: {
           onShowKeyboardDropWarning: null,
           onSelect: null,
           onBeginMove: "preventable",
           onMove: null,
           afterMove: null,
           onHover: null
        },
        
        mergePolicy: {
            keysets: "replace",
            "selectors.selectables": "selectors.movables",
            "selectors.dropTargets": "selectors.movables"
        }
    });
    
    function firstSelectable(that) {
        var selectables = that.dom.fastLocate("selectables");
        if (selectables.length <= 0) {
            return null;
        }
        return selectables[0];
    }
    
    function bindHandlersToContainer(container, keyDownHandler, keyUpHandler, mouseMoveHandler) {
        container.keydown(keyDownHandler);
        container.keyup(keyUpHandler);
        // FLUID-143. Disable text selection for the reorderer.
        // ondrag() and onselectstart() are Internet Explorer specific functions.
        // Override them so that drag+drop actions don't also select text in IE.
        if (jQuery.browser.msie) {
            container[0].ondrag = function () { return false; }; 
            container[0].onselectstart = function () { return false; };
        } 
    }
    
    function addRolesToContainer(that) {
        var first = (that.dom.fastLocate("selectables")[0]);
        if (first) {
            that.container.ariaState("activedescendent", first.id);
        }
        that.container.ariaRole(that.options.containerRole.container);
        that.container.ariaState("multiselectable", "false");
        that.container.ariaState("readonly", "false");
        that.container.ariaState("disabled", "false");
    }
    
    function changeSelectedToDefault(jItem, styles) {
        jItem.removeClass(styles.selected);
        jItem.addClass(styles.defaultStyle);
        jItem.ariaState("selected", "false");
    }
    

    function createAvatarId (parentId) {
        // Generating the avatar's id to be containerId_avatar
        // This is safe since there is only a single avatar at a time
        return parentId + "_avatar";
    }
    
    var adaptKeysets = function (options) {
        if (options.keysets && !(options.keysets instanceof Array)) {
            options.keysets = [options.keysets];    
        }
    };
    
    /**
     * @param container - the root node of the Reorderer.
     * @param options - an object containing any of the available options:
     *                  containerRole - indicates the role, or general use, for this instance of the Reorderer
     *                  keysets - an object containing sets of keycodes to use for directional navigation. Must contain:
     *                            modifier - a function that returns a boolean, indicating whether or not the required modifier(s) are activated
     *                            up
     *                            down
     *                            right
     *                            left
     *                  styles - an object containing class names for styling the Reorderer
     *                                  defaultStyle
     *                                  selected
     *                                  dragging
     *                                  hover
     *                                  dropMarker
     *                                  mouseDrag
     *                                  avatar
     *                  avatarCreator - a function that returns a valid DOM node to be used as the dragging avatar
     */
    fluid.reorderer = function (container, options) {
        if (!container) {
            fluid.fail("Reorderer initialised with no container");
        }
        var thatReorderer = fluid.initView("fluid.reorderer", container, options);
        options = thatReorderer.options;
        
        var dropManager = fluid.dropManager();
        
        thatReorderer.layoutHandler = fluid.initSubcomponent(thatReorderer,
            "layoutHandler", [container, options, dropManager, thatReorderer.dom]);
        
        thatReorderer.activeItem = undefined;

        adaptKeysets(options);
 
        var kbDropWarning = thatReorderer.locate("dropWarning")
        var mouseDropWarning;
        if (kbDropWarning) {
            mouseDropWarning = kbDropWarning.clone();
        }

        var isMove = function (evt) {
            var keysets = options.keysets;
            for (var i = 0; i < keysets.length; i++) {
                if (keysets[i].modifier(evt)) {
                    return true;
                }
            }
            return false;
        };
        
        var isActiveItemMovable = function () {
            return jQuery.inArray(thatReorderer.activeItem, thatReorderer.dom.fastLocate("movables")) >= 0;
        };
        
        var setDropEffects = function (value) {
            thatReorderer.dom.fastLocate("dropTargets").ariaState("dropeffect", value);
        };
        
        var styles = options.styles;
        
        thatReorderer.handleKeyDown = function(evt) {
            if (!thatReorderer.activeItem || thatReorderer.activeItem !== evt.target) {
                return true;
            }
            // If the key pressed is ctrl, and the active item is movable we want to restyle the active item.
            var jActiveItem = jQuery(thatReorderer.activeItem);
            if (!jActiveItem.hasClass(styles.dragging) && isMove(evt)) {
               // Don't treat the active item as dragging unless it is a movable.
                if (isActiveItemMovable()) {
                    jActiveItem.removeClass(styles.selected);
                    jActiveItem.addClass(styles.dragging);
                    jActiveItem.ariaState("grab", "true");
                    setDropEffects("move");
                }
                return false;
            }
            // The only other keys we listen for are the arrows.
            return thatReorderer.handleDirectionKeyDown(evt);
        };

        thatReorderer.handleKeyUp = function(evt) {
            if (!thatReorderer.activeItem || thatReorderer.activeItem !== evt.target) {
                return true;
            }
            var jActiveItem = jQuery(thatReorderer.activeItem);
            
            // Handle a key up event for the modifier
            if (jActiveItem.hasClass(styles.dragging) && !isMove(evt)) {
                if (kbDropWarning) {
                    kbDropWarning.hide();
                }
                jActiveItem.removeClass(styles.dragging);
                jActiveItem.addClass(styles.selected);
                jActiveItem.ariaState("grab", "supported");
                setDropEffects("none");
                return false;
            }
            
            return false;
        };

        
        var noModifier = function (evt) {
            return (!evt.ctrlKey && !evt.altKey && !evt.shiftKey && !evt.metaKey);
        };
        
        thatReorderer.handleDirectionKeyDown = function (evt) {
            var item = thatReorderer.activeItem;
            if (!item) {
                return true;
            }
            var keysets = options.keysets;
            for (var i = 0; i < keysets.length; i++) {
                var keyset = keysets[i];
                var didProcessKey = false;
                var keydir = fluid.utils.findKey(keyset, evt.keyCode);
                if (!keydir) {
                    continue;
                }
                var isMovement = keyset.modifier(evt);
                
                var dirnum = fluid.keycodeDirection[keydir];
                var relativeItem = thatReorderer.layoutHandler.getRelativePosition(item, dirnum, !isMovement);
                if (!relativeItem) {
                    continue;
                }
                
                if (isMovement) {
                    var prevent = thatReorderer.events.onBeginMove.fire(item);
                    if (prevent) return false;
                    if (kbDropWarning) {
                        if (relativeItem.clazz === "locked") {
                            thatReorderer.events.onShowKeyboardDropWarning.fire(item, kbDropWarning);
                            kbDropWarning.show();                       
                        }
                        else {
                            kbDropWarning.hide();
                        }
                    }
                    if (relativeItem.element) {
                        thatReorderer.requestMovement(relativeItem, item);
                    }
            
                } else if (noModifier(evt)) {
                    jQuery(relativeItem.element).focus();
                }
                return false;
            }
            return true;
        };

        var dropMarker;

        var createDropMarker = function (tagName) {
            var dropMarker = jQuery(document.createElement(tagName));
            dropMarker.addClass(options.styles.dropMarker);
            dropMarker.hide();
            return dropMarker;
        };

        fluid.utils.setLogging(true);

        thatReorderer.requestMovement = function(requestedPosition, item) {
          // Temporary censoring to get around ModuleLayout inability to update relative to self.
            if (!requestedPosition || fluid.unwrap(requestedPosition.element) === fluid.unwrap(item)) {
                return;
            }
            thatReorderer.events.onMove.fire(item, requestedPosition);
            dropManager.geometricMove(item, requestedPosition.element, requestedPosition.position);
            //jQuery(thatReorderer.activeItem).removeClass(options.styles.selected);
           
            // refocus on the active item because moving places focus on the body
            thatReorderer.activeItem.focus();
            
            thatReorderer.refresh();
            
            dropManager.updateGeometry(thatReorderer.layoutHandler.getGeometricInfo());

            thatReorderer.events.afterMove.fire(item, requestedPosition, thatReorderer.dom.fastLocate("movables"));
        };

        var hoverStyleHandler = function(item, state) {
            thatReorderer.dom.fastLocate("grabHandle", item)[state?"addClass":"removeClass"](styles.hover);
        };
        /**
         * Takes a jQuery object and adds 'movable' functionality to it
         */
        function initMovable(item) {
            var styles = options.styles;
            item.ariaState("grab", "supported");

            item.mouseover(
                function () {
                    thatReorderer.events.onHover.fire(item, true);
                }
            );
        
            item.mouseout(
                function () {
                    thatReorderer.events.onHover.fire(item, false);
                }
            );
            var avatar;
        
            item.draggable({
                refreshPositions: false,
                scroll: true,
                helper: function () {
                    var dropWarningEl;
                    if (mouseDropWarning) {
                        dropWarningEl = mouseDropWarning[0];
                    }
                    avatar = jQuery(options.avatarCreator(item[0], styles.avatar, dropWarningEl));
                    avatar.attr("id", createAvatarId(thatReorderer.container.id));
                    return avatar;
                },
                start: function (e, ui) {
                    var prevent = thatReorderer.events.onBeginMove.fire(item);
                    if (prevent) return false;
                    var handle = thatReorderer.dom.fastLocate("grabHandle", item)[0];
                    var handlePos = fluid.utils.computeAbsolutePosition(handle);
                    var handleWidth = handle.offsetWidth;
                    var handleHeight = handle.offsetHeight;
                    item.focus();
                    item.removeClass(options.styles.selected);
                    item.addClass(options.styles.mouseDrag);
                    item.ariaState("grab", "true");
                    setDropEffects("move");
                    dropManager.startDrag(e, handlePos, handleWidth, handleHeight);
                    avatar.show();
                },
                stop: function(e, ui) {
                    item.removeClass(options.styles.mouseDrag);
                    item.addClass(options.styles.selected);
                    jQuery(thatReorderer.activeItem).ariaState("grab", "supported");
                    var markerNode = fluid.unwrap(dropMarker);
                    if (markerNode.parentNode) {
                        markerNode.parentNode.removeChild(markerNode);
                    }
                    avatar.hide();
                    ui.helper = null;
                    setDropEffects("none");
                    dropManager.endDrag();
                    
                    thatReorderer.requestMovement(dropManager.lastPosition(), item);
                    // refocus on the active item because moving places focus on the body
                    thatReorderer.activeItem.focus();
                },
                handle: thatReorderer.dom.fastLocate("grabHandle", item)
            });
        }
           
       var selectItem = function (anItem) {
           thatReorderer.events.onSelect.fire(anItem);
           var styles = options.styles;
           // Set the previous active item back to its default state.
           if (thatReorderer.activeItem && thatReorderer.activeItem !== anItem) {
               changeSelectedToDefault(jQuery(thatReorderer.activeItem), styles);
           }
           // Then select the new item.
           thatReorderer.activeItem = anItem;
           var jItem = jQuery(anItem);
           jItem.removeClass(styles.defaultStyle);
           jItem.addClass(styles.selected);
           jItem.ariaState("selected", "true");
           thatReorderer.container.ariaState("activedescendent", anItem.id);
       };
   
       var initSelectables = function () {
            var handleBlur = function (evt) {
                changeSelectedToDefault(jQuery(this), options.styles);
                return evt.stopPropagation();
            };
        
            var handleFocus = function (evt) {
                selectItem(this);
                return evt.stopPropagation();
            };
            
            var selectables = thatReorderer.dom.fastLocate("selectables");
            selectables.addClass(styles.defaultStyle);
            
            selectables.blur(handleBlur);
            selectables.focus(handleFocus);
            selectables.click(function(evt) {selectItem(this)});
            
            selectables.ariaRole(options.containerRole.item);
            selectables.ariaState("selected", "false");
            selectables.ariaState("disabled", "false");
                
            thatReorderer.container.selectable(
              {selectableElements: selectables,
               selectablesTabindex: thatReorderer.options.selectablesTabindex,
               direction: null});
            thatReorderer.selectableContext = thatReorderer.container.getSelectableContext();
        };
    
        var dropChangeListener = function(dropTarget) {
             fluid.moveDom(dropMarker, dropTarget.element, dropTarget.position);
             dropMarker.css("display","");
             if (mouseDropWarning) {
                 if (dropTarget.lockedelem) {
                     mouseDropWarning.show();
                     }
                 else {
                     mouseDropWarning.hide();
                 }
             }
        };
    
        var initItems = function () {
            var movables = thatReorderer.dom.fastLocate("movables");
            var dropTargets = thatReorderer.dom.fastLocate("dropTargets");
            initSelectables();
        
            // Setup movables
            for (var i = 0; i < movables.length; i++) {
                var item = movables[i];
                initMovable(jQuery(item));
            }

            // In order to create valid html, the drop marker is the same type as the node being dragged.
            // This creates a confusing UI in cases such as an ordered list. 
            // drop marker functionality should be made pluggable. 
            if (movables.length > 0) {
                dropMarker = createDropMarker(movables[0].tagName);
            }
            
            dropManager.updateGeometry(thatReorderer.layoutHandler.getGeometricInfo());
            
            dropManager.dropChangeFirer.addListener(dropChangeListener);
            // Setup dropTargets
            dropTargets.ariaState("dropeffect", "none");
        };

        // Final initialization of the Reorderer at the end of the construction process 
        if (thatReorderer.container) {
            bindHandlersToContainer(thatReorderer.container, 
                thatReorderer.handleKeyDown,
                thatReorderer.handleKeyUp);
            addRolesToContainer(thatReorderer);
            thatReorderer.container.tabbable();
            initItems();
        }

       if (options.afterMoveCallbackUrl) {
            thatReorderer.events.afterMove.addListener(
              function () {
                  var layoutHandler = thatReorderer.layoutHandler;
                  var model = layoutHandler.getModel? layoutHandler.getModel():
                        options.acquireModel(thatReorderer);
                  jQuery.post(options.afterMoveCallbackUrl, JSON.stringify(model));
            }, "postModel");
       }
       thatReorderer.events.onHover.addListener(hoverStyleHandler, "style");
       
       thatReorderer.refresh = function() {
           thatReorderer.dom.refresh("movables");
           thatReorderer.dom.refresh("selectables");
           thatReorderer.dom.refresh("grabHandle", thatReorderer.dom.fastLocate("movables"));
           thatReorderer.dom.refresh("stylisticOffset", thatReorderer.dom.fastLocate("movables"));
           thatReorderer.dom.refresh("dropTargets");
           thatReorderer.selectableContext.selectables = thatReorderer.dom.fastLocate("selectables");
           thatReorderer.selectableContext.selectablesUpdated(thatReorderer.activeItem);
       };
       
       thatReorderer.refresh();
           
       return thatReorderer;
       };
    
    // Simplified API for reordering lists and grids.
    var simpleInit = function (container, layoutHandler, options) {
        options = options || {};
        options.layoutHandler = layoutHandler;
        return fluid.reorderer(container, options);
    };
    
    fluid.reorderList = function (container, options) {
        return simpleInit(container, "fluid.listLayoutHandler", options);
    };
    
    fluid.reorderGrid = function (container, options) {
        return simpleInit(container, "fluid.gridLayoutHandler", options); 
    };
    
    fluid.reorderer.GEOMETRIC_STRATEGY   = "projectFrom";
    fluid.reorderer.LOGICAL_STRATEGY     = "logicalFrom";
    fluid.reorderer.WRAP_LOCKED_STRATEGY = "lockedWrapFrom"
    fluid.reorderer.NO_STRATEGY = null;
    
    fluid.reorderer.relativeInfoGetter = function(orientation, coStrategy, contraStrategy, dropManager, dom) {
        return function(item, direction, forSelection) {
            var dirorient = fluid.directionOrientation(direction);
            var strategy = dirorient === orientation? coStrategy: contraStrategy;
            return strategy !== null? dropManager[strategy](item, direction, forSelection) : null;
        };
    };
    
}) (jQuery, fluid);

/*******************
 * Layout Handlers *
 *******************/
(function (jQuery, fluid) {

    function geometricInfoGetter(orientation, dom) {
        return function() {
           return {
               extents: [{orientation : orientation, 
                         elements     : dom.fastLocate("dropTargets")
                         }
                         ],
                elementMapper: function(element) {
                        return jQuery.inArray(element, dom.fastLocate("movables")) === -1? "locked": null;
                        }};
        };
    }
    
    fluid.defaults(true, "fluid.listLayoutHandler", 
        {orientation:         fluid.orientation.VERTICAL,
         containerRole:       fluid.roles.LIST,
         selectablesTabindex: -1
        });
    
    // Public layout handlers.
    fluid.listLayoutHandler = function (container, options, dropManager, dom) {
        var that = {};

        that.getRelativePosition = 
          fluid.reorderer.relativeInfoGetter(options.orientation, 
                fluid.reorderer.LOGICAL_STRATEGY, null, dropManager, dom);
        
        that.getGeometricInfo = geometricInfoGetter(options.orientation, dom);
        
        return that;
    }; // End ListLayoutHandler

    fluid.defaults(true, "fluid.gridLayoutHandler", 
        {orientation:        fluid.orientation.HORIZONTAL,
         containerRole:      fluid.roles.GRID,
         selectablesTabindex: -1
         });
    /*
     * Items in the Lightbox are stored in a list, but they are visually presented as a grid that
     * changes dimensions when the window changes size. As a result, when the user presses the up or
     * down arrow key, what lies above or below depends on the current window size.
     * 
     * The GridLayoutHandler is responsible for handling changes to this virtual 'grid' of items
     * in the window, and of informing the Lightbox of which items surround a given item.
     */
    fluid.gridLayoutHandler = function (container, options, dropManager, dom) {
        var that = {};

        that.getRelativePosition = 
           fluid.reorderer.relativeInfoGetter(options.orientation, 
                 fluid.reorderer.LOGICAL_STRATEGY, fluid.reorderer.GEOMETRIC_STRATEGY, 
                 dropManager, dom);
        
        that.getGeometricInfo = geometricInfoGetter(options.orientation, dom);
        
        return that;
    }; // End of GridLayoutHandler

}) (jQuery, fluid);
