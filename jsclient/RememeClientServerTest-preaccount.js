preaccount_Tests =
[

    { title: 'Get User Info Unvalidated fail (not exists)'
       , enabled: true
       , test: ooTest("getUserInfo", {}, function(rs, rx) { return rs.error === 1 } )
       },

    { title: 'Request a new existing user fail'
       , enabled: true
       , test: okTest("requestNewUserAccount", {}, function(rs, rx) { return rs.error === 6 } )
       },

    { title: 'Request a non existing user create one'
       , enabled: true
       , test: ooTest("requestNewUserAccount", {}, function(rs, rx) { return rs.status === "Account validation code has been sent to your mail successfully" } )
       },

    { title: 'Recently created user is unvalidated (can not request operation)'
       , enabled: true
       , test: ooTest("getUserInfo", {}, function(rs, rx) { return rs.error === 2 } )
       },

];
