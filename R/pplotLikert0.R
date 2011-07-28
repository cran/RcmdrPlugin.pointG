pplotLikert0<-function () 
{
    initializeDialog(title = gettextRcmdr("Batteries de Likert"))
    xBox <- variableListBox(top, Numeric(), selectmode = "multiple", 
        title = gettextRcmdr("Choisir vos variables"))
    identifyVariable <- tclVar("0")
    identifyFrame <- tkframe(top)
    .groups <- FALSE
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
    groupsBox(boxPlot)
    OKCancelHelp(helpSubject = "plotLikert")
    tkgrid(getFrame(xBox), sticky = "nw")
    tkgrid(identifyFrame, stick = "w")
    tkgrid(groupsFrame, sticky = "w")
    tkgrid(buttonsFrame, sticky = "w")
    dialogSuffix(rows = 4, columns = 1)
}
