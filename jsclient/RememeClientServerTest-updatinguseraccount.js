updatinguseraccount_Tests =
[

   { title: 'A validated user account can be changed'
       , enabled: true
       , test: okTest("updateUserInfo", {name: "My new name"}, function(rs, rx) { return rs.status === 'Account updated successfully' } )
       },

   { title: 'The user information has changed'
       , enabled: true
       , test: okTest("getUserInfo", {}, function(rs, rx) { return rs.name === "My new name" } )
       },

   { title: 'A wrong user account can not be changed'
       , enabled: true
       , test: koTest("updateUserInfo", {name: "My new name"}, function(rs, rx) { return rs.error === 1 } )
       },

   { title: 'A unvalidated user account can not be changed'
       , enabled: true
       , test: ooTest("updateUserInfo", {name: "My new 2 name"}, function(rs, rx) { return rs.error === 2 } )
       },

];