ppplotLikert0 <- function () {
	defaults <- list(initial.x = NULL, initial.identifyPoints = 0, initialGroup=NULL) 
	dialog.values <- getDialog("boxPlot", defaults)
	initializeDialog(title = gettextRcmdr("Graphique de Likert"))
	xBox <- variableListBox(top, Numeric(),selectmode = "multiple", title = gettextRcmdr("Choisir vos variables (de Likert)"), 
			initialSelection = varPosn (dialog.values$initial.x, "numeric"))
	identifyVariable <- tclVar(dialog.values$initial.identifyPoints)
	identifyFrame <- tkframe(top)
	identifyCheckBox <- tkcheckbutton(identifyFrame, variable = identifyVariable)
	initial.group <- dialog.values$initial.group
	.groups <- if (is.null(initial.group)) FALSE else initial.group
onOK <- function() {
        x <- getSelection(xBox)
        closeDialog()
        if (length(x) == 0) {
            errorCondition(recall = pplotLikert0, message = gettextRcmdr("You must select a variable"))
            return()
        }
        .activeDataSet <- ActiveDataSet()
        var <- paste(.activeDataSet, "$", x, sep = "")
        if (.groups == FALSE) {
            selectVars <- paste(", select=c(", paste(x, collapse = ","), 
                ")", sep = "")
            newn <- "D1"
            command <- paste(newn, " <- subset(", ActiveDataSet(), 
                selectVars, ")", sep = "")
            logger(command)
            result <- justDoIt(command)
            plotLikert0("D1", yy = FALSE)
        }
        else {
            selectVars <- paste(", select=c(", .groups, ",", 
                paste(x, collapse = ","), ")", sep = "")
            newn <- "D1"
            command <- paste(newn, " <- subset(", ActiveDataSet(), 
                selectVars, ")", sep = "")
            logger(command)
            result <- justDoIt(command)
            print(plotLikert0(newn, yy = TRUE))
        }
        activateMenus()
        tkfocus(CommanderWindow())
    }
	groupsBox(boxPlot, initialGroup=initial.group, 
			initialLabel=if (is.null(initial.group)) gettextRcmdr("Plot by groups") else paste(gettextRcmdr("Plot by:"), initial.group))
	OKCancelHelp(helpSubject = "boxplot", reset = "ppplotLikert0")
	tkgrid(getFrame(xBox), sticky = "nw")
	tkgrid(labelRcmdr(identifyFrame, text = gettextRcmdr("Identify outliers with mouse"), 
					justify = "left"), identifyCheckBox, sticky = "w")
	tkgrid(identifyFrame, stick = "w")
	tkgrid(groupsFrame, sticky = "w")
	tkgrid(buttonsFrame, sticky = "w")
	dialogSuffix(rows = 4, columns = 1)
}

