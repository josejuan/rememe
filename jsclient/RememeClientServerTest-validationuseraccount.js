validationuseraccount_Tests =
[

   { title: 'A validated user account can not be validated'
       , enabled: true
       , test: okTest( "validateUserAccount", "validating", function(rs, rx) { return rs.error === 3 } )
       },

   { title: 'A invalid user account can not be validated'
       , enabled: true
       , test: koTest("validateUserAccount", "validating", function(rs, rx) { return rs.error === 1 } )
       },

   { title: 'A unvalidated user account can not be validated with a wrong validation code'
       , enabled: true
       , test: ooTest("validateUserAccount", "BAD_validation_code", function(rs, rx) { return rs.error === 4 } )
       },

   { title: 'A unvalidated user account can be validated with a correct validation code'
       , enabled: true
       , test: function () {
                  var vcode = prompt("Check your mail `" + ooCnx.usr + "` and enter the validation code");
                  var testc = ooTest("validateUserAccount", vcode, function(rs, rx) { return rs.status === "Account validated successfully" } );
                  return testc();
               }
       },

   { title: 'Recently validated user account can be retrieved'
       , enabled: true
       , test: ooTest("getUserInfo", {}, function(rs, rx) { return rs.email === ooCnx.usr } )
       },

];
