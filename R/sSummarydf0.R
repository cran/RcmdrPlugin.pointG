sSummarydf0<-function () 
{
    newName <- activeDataSet()
    initializeDialog(title = gettextRcmdr("En resume..."))
    allVariablesFrame <- tkframe(top)
    allVariables <- tclVar("1")
    allVariablesCheckBox <- tkcheckbutton(allVariablesFrame, 
        variable = allVariables)
    variablesBox <- variableListBox(top, Variables(), selectmode = "multiple", 
        initialSelection = NULL, title = gettextRcmdr("Variables (select one or more)"))
    subsetVariable <- tclVar(gettextRcmdr("<all cases>"))
    onOK <- function() {
        if (!is.valid.name(newName)) {
            errorCondition(recall = ssummary.df0, message = paste("\"", 
                newName, "\" ", gettextRcmdr("is not a valid name."), 
                sep = ""))
            return()
        }
        selectVars <- if (tclvalue(allVariables) == "1") 
            ""
        else {
            x <- getSelection(variablesBox)
            if (0 == length(x)) {
                errorCondition(recall = sSummarydf0, message = gettextRcmdr("No variables were selected."))
                return()
            }
            paste(", select=c(", paste(x, collapse = ","), ")", 
                sep = "")
        }
        closeDialog()
        cases <- tclvalue(subsetVariable)
        selectCases <- if (cases == gettextRcmdr("<all cases>")) 
            ""
        else paste(", subset=", cases, sep = "")
        if (selectVars == "" && selectCases == "") {
            dfdf <- get(newName)
            print(Summarydf0(dfdf))
            return()
        }
        newn <- "D1"
        command <- paste(newn, " <- subset(", ActiveDataSet(), 
            selectCases, selectVars, ")", sep = "")
        logger(command)
        result <- justDoIt(command)
        dfdf <- get(newn)
        print(Summarydf0(dfdf))
        if (class(result)[1] != "try-error") 
            tkfocus(CommanderWindow())
    }
    OKCancelHelp(helpSubject = "subset")
    tkgrid(labelRcmdr(allVariablesFrame, text = gettextRcmdr("Include all variables")), 
        allVariablesCheckBox, sticky = "w")
    tkgrid(allVariablesFrame, sticky = "w")
    tkgrid(labelRcmdr(top, text = gettextRcmdr("   OR"), fg = "red"), 
        sticky = "w")
    tkgrid(getFrame(variablesBox), sticky = "nw")
    tkgrid(buttonsFrame, sticky = "w")
    dialogSuffix(rows = 6, columns = 1)
}
