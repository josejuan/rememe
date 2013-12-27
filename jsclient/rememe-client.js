function RememeClient(url, usr, pwd, async = false) {
    this.url = url;
    this.usr = usr;
    this.pwd = pwd;
    this.async = async;

    this.fakeUser = { email:          "(no email)"
                    , password:       "(no password)"
                    , validated:      false
                    , uuidValidation: "(no uuid)"
                    };
    this.fakeNote = { key: 0
                    , value: { userId:         0
                             , title:          "(no title)"
                             , description:    "(no description)"
                             , alarmAt:        new Date()
                             , alarmOn:        false
                             , hidden:         false
                             }
                    };

    this.pure =
        function (msg, onOk, onKo) {
            var xhr = new XMLHttpRequest();
            xhr.open('POST', this.url, this.async);
            xhr.onload = // TODO: control network errors (abort, ...)
                function() {
                    if(this.status !== 200)
                        onKo(this, this.statusText);
                    else {
                        var o = JSON.parse(this.responseText);
                        if(typeof o.error === "number")
                            onKo(this, [o.description, o.context].join('. '));
                        else
                            onOk(o);
                    }
                };
            xhr.send(JSON.stringify(msg));
        };

    this.call =
        function (action, msg, onOk, onKo) {
            msg.action   = action;
            msg.usermail = this.usr;
            msg.password = this.pwd;
            this.pure(msg, onOk, onKo);
        };

    this.withDefaults =
        function (a, b) {
            var o = this;
            Object.keys(b).map(function(k) {
                if(typeof a[k] == "undefined")
                    a[k] = b[k];
                if(typeof b[k] === "object" && b[k] !== null)
                    o.withDefaults(a[k], b[k]);
            });
            return a;
        }

    this.getUserInfo =
        function (_, onOk, onKo) {
            this.call("userinfo", {}, onOk, onKo);
        };

    this.requestNewUserAccount =
        function (_, onOk, onKo) {
            this.call("useradd", {}, onOk, onKo);
        };

    this.validateUserAccount =
        function (validationCode, onOk, onKo) {
            this.call("uservalidate", {validationCode: validationCode}, onOk, onKo);
        };

    this.updateUserInfo =
        function (userdata, onOk, onKo) {
            var userdata_ = this.withDefaults(userdata, this.fakeUser);
            this.call("userupdate", {userdata: userdata_}, onOk, onKo);
        };

    this.addNote =
        function (notedata, onOk, onKo) {
            var notedata_ = this.withDefaults(notedata, this.fakeNote.value);
            this.call("noteadd", {notedata: {key: 0, value: notedata_}}, onOk, onKo);
        };

    this.listNotes =
        function (noteFilter, onOk, onKo) {
            var noteFilter_ = this.withDefaults( noteFilter
                                               , { hiddenMustBe: null
                                                 , titleMustContains: null
                                                 });
            this.call("notelist", noteFilter, onOk, onKo);
        };

    this.updateNote =
        function (notedata, onOk, onKo) {
            var notedata_ = this.withDefaults(notedata, this.fakeNote);
            this.call("noteupdate", {notedata: notedata_}, onOk, onKo);
        };

    this.deleteNote =
        function (noteId, onOk, onKo) {
            var notedata_ = this.withDefaults({ key: noteId }, this.fakeNote);
            this.call("notedelete", {notedata: notedata_}, onOk, onKo);
        };

}
