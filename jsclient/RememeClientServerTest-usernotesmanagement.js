usernotesmanagement_Tests =
[

    { title: 'Valid user can create notes'
       , enabled: true
       , test: okTest( "addNote"
                     , { title: "My first note"
                       , description: "My first description"
                       , alarmAt: new Date()
                       , alarmOn: true
                       , hidden: false
                       }
                     , function(rs, rx) {
                         return rs.status === "Note created successfully";
                       }
                     )
       },

    { title: 'Invalid user can not create notes'
       , enabled: true
       , test: koTest( "addNote"
                     , { title: "My first note"
                       , description: "My first description"
                       , alarmAt: new Date()
                       , alarmOn: true
                       , hidden: false
                       }
                     , function(rs, rx) {
                         return rs.error === 1;
                       }
                     )
       },

    { title: 'Unvalidated user can not create notes'
       , enabled: true
       , test: ooTest( "addNote"
                     , { title: "My first note"
                       , description: "My first description"
                       , alarmAt: new Date()
                       , alarmOn: true
                       , hidden: false
                       }
                     , function(rs, rx) {
                         return rs.error === 2;
                       }
                     )
       },

    { title: 'Valid user can list their notes'
       , enabled: true
       , test: okTest( "listNotes"
                     , {}
                     , function(rs, rx) {
                         return rs.map(function(n){return n.value.title}).sort().join('|') === "My first note|bla, bla";
                       }
                     )
       },

    { title: 'Valid user can remove their notes'
       , enabled: true
       , test: okTest( "deleteNote", 2, function(rs, rx) { return rs.status === "Note data deleted successfully" } )
       },

    { title: 'Valid user can update their notes'
       , enabled: true
       , test: okTest( "updateNote"
                     , { key: 1
                       , value: { title: "My updated note"
                                , description: "My updated description"
                                , alarmAt: new Date()
                                , alarmOn: true
                                , hidden: false
                                }
                       }
                     , function(rs, rx) {
                         return rs.status === "Note updated successfully";
                       }
                     )
       },

    { title: 'Check update and delete notes'
       , enabled: true
       , test: okTest( "listNotes"
                     , {}
                     , function(rs, rx) {
                         return rs.map(function(n){return n.value.title}).sort().join('|') === "My updated note";
                       }
                     )
       },

];
