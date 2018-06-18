// itasks-sdk //////////////////////////////////////////////////////////////////
// 100 OnActions
//  30 OnValues
//   5 mixed
// --------------- +
// 135 total


// Examples/Applications/Incidone/Incidone/OP/CommunicationManagementTasks.icl:249
determineContact :: (Maybe ContactNo) -> Task ContactNo
determineContact mbPrevious
    =  withShared {ContactFilter|filterByName=Nothing}
      \filter ->
       createNewContact filter
       -||-
       selectExistingContact filter
       <<@ ArrangeVertical
where
    createNewContact filter
        =   enterInformation (Title "Contact") [] // @> (mapToFilter,filter)
        >>* [OnAction ActionCreate (hasValue (createContact))]
    selectExistingContact filter
        =   whileUnchanged filter
            \curFilter ->
            enterChoiceWithSharedAs (Title "Select contact") [ChooseFromList contactTitle] (sdsFocus curFilter filteredContactsShort) contactIdentity
        >>* [OnValue (hasValue return)
            :maybe [] (\contactNo -> [OnAction ActionCancel (always (return contactNo))]) mbPrevious]


// Libraries/iTasks/Extensions/Admin/TonicAdmin.icl:92
viewStaticTask :: !AllBlueprints ![TaskAppRenderer] !(Shared NavStack) !BlueprintIdent !TonicModule !TonicFunc !Int !Bool -> Task ()
viewStaticTask allbps rs navstack bpref tm tt depth compact
  =          get navstack
  >>~ \ns -> (showStaticBlueprint rs bpref (expandTask allbps depth tt) compact depth
         >>* [ OnValue (doAction (handleClicks tm tt))
             , OnAction (Action "Back") (navigateBackwards tm tt ns)
             ] @! ()) <<@ ApplyLayout (layoutSubUIs (SelectByType UIAction) (setActionIcon ('DM'.fromList [("Back","Previous")])))
  where
  navigateBackwards :: TonicModule TonicFunc NavStack a -> Maybe (Task ())
  navigateBackwards _  _  []           _ = Nothing
  navigateBackwards tm tt [prev:stack] _ = navigateBackwards` prev
    where
    navigateBackwards` :: ClickMeta -> Maybe (Task ())
    navigateBackwards` meta`=:{click_origin_mbbpident = Just {bpident_moduleName, bpident_compName, bpident_compId = Just tid}}
      =                 Just (upd pop navstack
      >>|               get dynamicDisplaySettings
      >>~ \sett ->      get selectedDetail
      >>~ \selDetail -> get (sdsFocus (comp2TaskId tid, bpident_moduleName, bpident_compName) tonicInstances)
      >>~ \mbpref ->    case mbpref of
                          Just bpref` -> viewInstance rs navstack sett bpref` selDetail meta`
                          _           -> return ())
    navigateBackwards` meta=:{click_origin_mbbpident = Just {bpident_moduleName, bpident_compName}}
      =   Just (upd pop navstack
      >>| getModule bpident_moduleName
      >>* [ OnValue (onNavVal bpident_compName)
          , OnAllExceptions (const (viewInformation "Error" [] "Something went wrong with navigating backwards" @! ()))
          ] @! ())
      where
      onNavVal bpident_compName (Value tm` _) = fmap (\tt` -> viewStaticTask allbps rs navstack {bpr_moduleName = bpident_moduleName, bpr_taskName = bpident_compName} tm` tt` depth compact @! ()) (getTonicFunc tm` bpident_compName)
      onNavVal _                _             = Nothing
    navigateBackwards` _ = Nothing
    pop [] = []
    pop [_:xs] = xs


// Libraries/iTasks/Extensions/Admin/WorkflowAdmin.icl:286
workOnTask :: !TaskId -> Task ()
workOnTask taskId
    =   (workOn taskId <<@ ApplyLayout (setUIAttributes (heightAttr FlexSize))
    >>* [OnValue    (ifValue ((===) ASExcepted) (\_ -> viewInformation (Title "Error") [] "An exception occurred in this task" >>| return ()))
        ,OnValue    (ifValue ((===) ASIncompatible) (\_ -> dealWithIncompatibleTask))
        ,OnValue    (ifValue ((===) ASDeleted) (\_ -> return ()))
        ,OnValue    (ifValue ((===) (ASAttached True)) (\_ -> return ())) //If the task is stable, there is no need to work on it anymore
        ,OnAction ActionClose   (always (return ()))
        ] ) <<@ ApplyLayout (copySubUIAttributes (SelectKeys ["title"]) [0] []) //Use the title from the workOn for the composition
where
    dealWithIncompatibleTask
        =   viewInformation (Title "Error") [] "This this task is incompatible with the current application version. Restart?"
        >>* [OnAction ActionYes (always restartTask)
            ,OnAction ActionNo (always (return ()))
            ]
    restartTask
        =   findReplacement taskId
        >>- \mbReplacement -> case mbReplacement of
            Nothing
                =   viewInformation (Title "Error") [] "Sorry, this task is no longer available in the workflow catalog"
                >>| return ()
            Just replacement
                =   replaceTask taskId (const (unwrapWorkflowTask replacement.Workflow.task)) topLevelTasks
                >>| workOnTask taskId
    //Look in the catalog for an entry that has the same path as
    //the 'catalogId' that is stored in the incompatible task instance's properties
    findReplacement taskId
        =  get (sdsFocus taskId (taskListEntryMeta topLevelTasks) |+| workflows)
        @  \(taskListEntry,catalog) -> maybe Nothing (lookup catalog) ('DM'.get "catalogId" taskListEntry.TaskListItem.attributes)
    where
        lookup [wf=:{Workflow|path}:wfs] cid = if (path == cid) (Just wf) (lookup wfs cid)
        lookup [] _ = Nothing


// Examples/BasicAPIExamples.icl:289
showAndDo fun ip =
    viewSharedInformation "In store" [] store
        ||-
    fun ip >>*
        [ OnValue (hasValue (\_ -> editSharedList store))
        , OnAction (Action "Cancel") (always (editSharedList store))
        ]


// Examples/Applications/ShipAdventure/C2/Apps/ShipAdventure/Core.icl:172
handleWhileWalking :: !MyActor !String !Priority -> Task ()
handleWhileWalking actor title priority
  =   addTaskForUser title actor.userName Immediate (const taskToHandle)
  >>* [ OnValue  (ifValue isDone (\x -> viewInformation ("Task " <+++ title <+++ " succeeded, returning:") [] x @! ()))
      , OnValue  (ifValue isFailed (\x -> viewInformation ("Task " <+++ title <+++ " failed, returning:") [] x @! ()))
      , OnAction (Action "Cancel task") (always (viewInformation "Canceled" [] ("Task " <+++ title <+++ " has been cancelled by you") @! ()))
      ]
  >>| return ()
  where
  taskToHandle
    =   moveAround mkSection actor.userName inventoryInSectionShare myStatusMap myUserActorMap myInventoryMap
    ||- taskToDo (alarmLoc, detector) actor.userName myStatusMap myUserActorMap myInventoryMap
  isDone (MoveDone _) = True
  isDone _            = False
  isFailed (MoveFailed _) = True
  isFailed _              = False
