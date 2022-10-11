function SetRadioButtonAsSelected(radioButtonId) {
    var radioButton = UI.GetById(radioButtonId);
    if (!radioButton) {
        return;
    }
    radioButton.checked = true;

    var radioButtonsForQuestion = document.getElementsByName(radioButton.name);
    if (!radioButtonsForQuestion) {
        return;
    }

    [].forEach.call(radioButtonsForQuestion, function (rb) {
        var siblingAncestors = D2L.LP.Web.UI.Html.Dom.GetAncestorElementsByClassName(
            'd2l-rowshadeonhover-selected',
            rb
        );

        if (siblingAncestors[0]) {
            siblingAncestors[0].classList.remove('d2l-rowshadeonhover-selected');
            siblingAncestors[0].classList.add('d2l-rowshadeonhover');
        }
    });

    var shadedAncestors = D2L.LP.Web.UI.Html.Dom.GetAncestorElementsByClassName(
        'd2l-rowshadeonhover',
        radioButton
    );

    if (!shadedAncestors || shadedAncestors.length !== 1) {
        return;
    }

    shadedAncestors[0].classList.remove('d2l-rowshadeonhover');
    shadedAncestors[0].classList.add('d2l-rowshadeonhover-selected');

    // click on the radio button to trigger auto save
    onClickRadioButtonAutoSaveEvent(radioButtonId);

}

function ToggleCheckBox(event, checkBoxId) {
    // Prevent the default checkbox event from occuring
    event.stopPropagation();
    event.preventDefault();

    // Use settimeout = 0 to only propogate checked setting after all other DOM changes have occurred.
    setTimeout(function () {

        var checkBox = UI.GetById(checkBoxId);
        if (!checkBox) {
            return;
        }

        var oldShadeClass = checkBox.checked ? 'd2l-rowshadeonhover-selected' : 'd2l-rowshadeonhover';
        var newShadeClass = checkBox.checked ? 'd2l-rowshadeonhover' : 'd2l-rowshadeonhover-selected';

        checkBox.checked = !checkBox.checked;

        var shadedAncestors = D2L.LP.Web.UI.Html.Dom.GetAncestorElementsByClassName(
            oldShadeClass,
            checkBox
        );
        if (!shadedAncestors || shadedAncestors.length !== 1) {
            return;
        }
        shadedAncestors[0].classList.remove(oldShadeClass);
        shadedAncestors[0].classList.add(newShadeClass);

        // click on the checkbox to trigger auto save
        onClickCheckboxAutoSaveEvent(checkBoxId);
    }, 0);
}

function deleteMultiSelectTooltip() {
    var tooltips = document.getElementsByClassName('d2l_ms_limited_answer_tooltip');
    for (var i = 0; i < tooltips.length; i++) {
        if (tooltips[i]) {
            tooltips[i].remove();
        }
    }
}

function deleteMultiSelectTooltipEventListener() {
    deleteMultiSelectTooltip();
    document.removeEventListener('click', deleteMultiSelectTooltipEventListener);
}

//An expanded version of the ToggleCheckbox function, including the same logic to bypass checking the box is the 
//max number of allowed selections has already been made, for the "Correct Answers, Limited Selection" grading type
//of MultiSelect questions
function TryToggleCheckBoxForLimitedSelectionQuestion(event, checkBoxId, labelId, labelSid, numExpectedAnswers, message, style) {

    // Prevent the default checkbox event from occuring
    event.stopPropagation();
    event.preventDefault();
    // Use settimeout = 0 to only propogate checked setting after all other DOM changes have occurred.
    setTimeout(function () {

        var checkBox;
        if (style == 'Vertical') {
            checkBox = UI.GetById(checkBoxId);
            if (!checkBox) {
                return;
            }
        } else {
            checkBox = event.target;
            if (!checkBox || !checkBox.classList.contains('d2l-checkbox')) {
                return;
            }
        }
        var label = UI.GetControl(labelId, labelSid);
        var ancestorFieldset = findAncestorFieldset(label.IDomNode);

        // clear any existing 
        deleteMultiSelectTooltip();

        //logic to see if this checkbox is allowed to be checked
        if (!checkBox.checked) {
            if (ancestorFieldset !== null) {
                var countChecked = ancestorFieldset.querySelectorAll("input[type='checkbox']:checked").length;
                if (countChecked >= numExpectedAnswers) {
                    //checking another box is not allowed, so show a tooltip message saying so, 
                    //and return rather than proceeding to allow the checkbox to be marked checked                      
                    tooltip = document.createElement('div');
                    tooltip.setAttribute('id', 'd2l_ms_limited_answer_tooltip_' + checkBox.id);                   
                    tooltip.className = "d2l_ms_limited_answer_tooltip vui-validation-bubble vui-validation-bubble-show";
                    tooltip.setAttribute('aria-describedby', 'd2l_ms_limited_answer_tooltip_content_' + checkBox.id);
                    // The role is supposed to be 'tooltip' but the announcement only happened once in chrome when clicking the checkbox, 
                    // and subsequent clicking on the same checkbox would not trigger the announcement again. Also in firefox tabbing back
                    // to the previous checkbox would not trigger announcement. By setting the role to alert fixed the above issues. 
                    tooltip.setAttribute('role', 'alert');
                    
                    var tooltipContent = document.createElement('div');
                    tooltipContent.setAttribute('id', 'd2l_ms_limited_answer_tooltip_content_' + checkBox.id);
                    tooltipContent.className = "vui-validation-bubble-content";
                    tooltipContent.appendChild(document.createTextNode(message));
                    tooltip.appendChild(tooltipContent);

                    checkBox.addEventListener('blur', function () {
                        if (tooltip) {
                            tooltip.remove()
                        }
                    });
                    document.removeEventListener('click', deleteMultiSelectTooltipEventListener);
                    document.addEventListener('click', deleteMultiSelectTooltipEventListener);

                    if (checkBox.parentNode) {
                        checkBox.parentNode.appendChild(tooltip);
                    }
                    
                    return;
                }
            }
        }

        // passed the validation check, so proceed to check the box
        var oldShadeClass = checkBox.checked ? 'd2l-rowshadeonhover-selected' : 'd2l-rowshadeonhover';
        var newShadeClass = checkBox.checked ? 'd2l-rowshadeonhover' : 'd2l-rowshadeonhover-selected';

        // if box is getting unchecked, remove "d2l-checkbox-not-allowed" class from unchecked boxes
        if (checkBox.checked) {
            var unchecked = ancestorFieldset.querySelectorAll("input[type='checkbox']:not(:checked)");
            removeClassFromElementSet(unchecked, unchecked.length, "d2l-checkbox-not-allowed");
            document.removeEventListener('click', deleteMultiSelectTooltipEventListener);
        }

        checkBox.checked = !checkBox.checked;

        if (style == 'Vertical') {
            var shadedAncestors = D2L.LP.Web.UI.Html.Dom.GetAncestorElementsByClassName(
                oldShadeClass,
                checkBox
            );
            if (!shadedAncestors || shadedAncestors.length !== 1) {
                return;
            }
            shadedAncestors[0].classList.remove(oldShadeClass);
            shadedAncestors[0].classList.add(newShadeClass);
        }

       
        var countChecked = ancestorFieldset.querySelectorAll("input[type='checkbox']:checked").length;
        // if selection limit has been reached, add "d2l-checkbox-not-allowed" class to the remaining unchecked boxes
        if (countChecked >= numExpectedAnswers) {
            var unchecked = ancestorFieldset.querySelectorAll("input[type='checkbox']:not(:checked)");
            if (unchecked.length !== 0) {
                addClassToElementSet(unchecked, unchecked.length, "d2l-checkbox-not-allowed");                
            }

        }

        // click on the checkbox to trigger auto save
        onClickCheckboxAutoSaveEvent(checkBox.id);
    }, 0);
}

// A function to update checkbox styling upon navigating to a question page with saved answers 
// if selection limit has been reached
function updateCheckboxStyling(checkBoxId, labelId, labelSid, numExpectedAnswers) {
    var label = UI.GetControl(labelId, labelSid);
    var ancestorFieldset = findAncestorFieldset(label.IDomNode);
    var checkBox = UI.GetById(checkBoxId);

    if (!checkBox) {
        return;
    }
    if (ancestorFieldset !== null) {
        var countCheck = ancestorFieldset.querySelectorAll("input[type='checkbox']:checked").length;
        if (countCheck >= numExpectedAnswers) {
            var unchecked = ancestorFieldset.querySelectorAll("input[type='checkbox']:not(:checked)");
            addClassToElementSet(unchecked, unchecked.length, "d2l-checkbox-not-allowed");
        }
    }
}
// helper function to add class to set of elements
function addClassToElementSet(set, size, newClass) {
    for (i = 0; i < size; i++) {
        set[i].classList.add(newClass);
    }
}

// helper function to remove class from set of elements
function removeClassFromElementSet(set, size, oldClass) {
    for (i = 0; i < size; i++) {
        set[i].classList.remove(oldClass);
    }
}

//helper function for counting the number of checked checkboxes in a fieldset
//finds the fieldset they belong to. Returns null if not found.
function findAncestorFieldset(elem) {

    if (elem === undefined || elem === null) {
        return null;
    }

    var parentElem = elem.parentNode;

    while (parentElem !== null) {
        if (parentElem.nodeName.toLowerCase() === "fieldset") {
            return parentElem;
        }
        parentElem = parentElem.parentNode;
    }

    return null;
}

function onClickCheckboxAutoSaveEvent(checkBoxId) {
    var checkBox = UI.GetById(checkBoxId);
    if (!checkBox) {
        return;
    }

    WindowEventManager.BC(checkBox, 'd2l-quiz-autosave-checkbox-custom-eventobj');
}

function onClickRadioButtonAutoSaveEvent(radioButtonId) {
    var radioButton = UI.GetById(radioButtonId);
    if (!radioButton) {
        return;
    }

    WindowEventManager.BC(radioButton, 'd2l-quiz-autosave-radiobutton-custom-eventobj');
}