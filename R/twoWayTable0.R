twoWayTable0<-function () 
{
    Library("abind")
    initializeDialog(title = gettextRcmdr("Two-Way Table"))
    variablesFrame <- tkframe(top)
    .factors <- Factors()
    rowBox <- variableListBox(variablesFrame, .factors, title = gettextRcmdr("Row variable (pick one)"))
    columnBox <- variableListBox(variablesFrame, .factors, title = gettextRcmdr("Column variable (pick one)"))
    subsetBox()
    onOK <- function() {
        row <- getSelection(rowBox)
        column <- getSelection(columnBox)
        if (length(row) == 0 || length(column) == 0) {
            errorCondition(recall = twoWayTable0, message = gettextRcmdr("You must select two variables."))
            return()
        }
        if (row == column) {
            errorCondition(recall = twoWayTable0, message = gettextRcmdr("Row and column variables are the same."))
            return()
        }
        percents <- as.character(tclvalue(percentsVariable))
        chisq <- tclvalue(chisqTestVariable)
        chisqComp <- tclvalue(chisqComponentsVariable)
        expected <- tclvalue(expFreqVariable)
        fisher <- tclvalue(fisherTestVariable)
        subset <- tclvalue(subsetVariable)
        subset <- if (trim.blanks(subset) == gettextRcmdr("<all valid cases>")) 
            ""
        else paste(", subset=", subset, sep = "")
        closeDialog()
        command <- paste("xtabs(~", row, "+", column, ", data=", 
            ActiveDataSet(), subset, ")", sep = "")
        logger(paste(".Table <- ", command, sep = ""))
        assign(".Table", justDoIt(command), envir = .GlobalEnv)
        doItAndPrint(".Table")
        if (percents == "row") 
            doItAndPrint("rowPercents(.Table) # Row Percentages")
        if (percents == "column") 
            doItAndPrint("colPercents(.Table) # Column Percentages")
        if (percents == "total") 
            doItAndPrint("totPercents(.Table) # Percentage of Total")
        if (chisq == 1) {
            command <- "chisq.test(.Table, correct=FALSE)"
            logger(paste(".Test <- ", command, sep = ""))
            assign(".Test", justDoIt(command), envir = .GlobalEnv)
            doItAndPrint(".Test")
            command <- "pem(.Table)"
            doItAndPrint(command)
            command <- "plotTabCont0(.Table)"
            justDoIt(command)
            if (expected == 1) 
                doItAndPrint(".Test$expected # Expected Counts")
            warnText <- NULL
            if (0 < (nlt1 <- sum(.Test$expected < 1))) 
                warnText <- paste(nlt1, gettextRcmdr("expected frequencies are less than 1"))
            if (0 < (nlt5 <- sum(.Test$expected < 5))) 
                warnText <- paste(warnText, "\n", nlt5, gettextRcmdr(" expected frequencies are less than 5"), 
                  sep = "")
            if (!is.null(warnText)) 
                Message(message = warnText, type = "warning")
            if (chisqComp == 1) {
                command <- "round(.Test$residuals^2, 2) # Chi-square Components"
                doItAndPrint(command)
            }
            logger("remove(.Test)")
            remove(.Test, envir = .GlobalEnv)
        }
        if (fisher == 1) 
            doItAndPrint("fisher.test(.Table)")
        logger("remove(.Table)")
        remove(.Table, envir = .GlobalEnv)
        tkfocus(CommanderWindow())
    }
    OKCancelHelp(helpSubject = "xtabs")
    radioButtons(name = "percents", buttons = c("rowPercents", 
        "columnPercents", "totalPercents", "nonePercents"), values = c("row", 
        "column", "total", "none"), initialValue = "none", labels = gettextRcmdr(c("Row percentages", 
        "Column percentages", "Percentages of total", "No percentages")), 
        title = gettextRcmdr("Compute Percentages"))
    checkBoxes(frame = "testsFrame", boxes = c("chisqTest", "chisqComponents", 
        "expFreq", "fisherTest"), initialValues = c("1", "0", 
        "0", "0"), labels = gettextRcmdr(c("Chi-square test of independence", 
        "Components of chi-square statistic", "Print expected frequencies", 
        "Fisher's exact test")))
    tkgrid(getFrame(rowBox), labelRcmdr(variablesFrame, text = "    "), 
        getFrame(columnBox), sticky = "nw")
    tkgrid(variablesFrame, sticky = "w")
    tkgrid(percentsFrame, sticky = "w")
    tkgrid(labelRcmdr(top, text = gettextRcmdr("Hypothesis Tests"), 
        fg = "blue"), sticky = "w")
    tkgrid(testsFrame, sticky = "w")
    tkgrid(subsetFrame, sticky = "w")
    tkgrid(buttonsFrame, sticky = "w")
    dialogSuffix(rows = 6, columns = 1)
}
