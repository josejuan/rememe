getuserinfoaccountstate_Tests =
[

    { title: 'Get User Info Ok'
       , enabled: true
       , test: okTest("getUserInfo", {}, function(rs, rx) { return rs.name === 'Test Name' } )
       },

    { title: 'Get User Info Ko'
       , enabled: true
       , test: koTest("getUserInfo", {}, function(rs, rx) { return rs.error === 1 } )
       },

    { title: 'Get User Info Unvalidated'
       , enabled: true
       , test: ooTest("getUserInfo", {}, function(rs, rx) { return rs.error === 2 } )
       },

];