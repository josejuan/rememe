function toHtml(text) {
    var e = document.createElement('span');
    e.textContent = text;
    return e.innerHTML;
}

function runTest(test) {
    if(!test.enabled)
        return {ok: 0, ko: 0, tm: 0, html: ''};
    var html = '', ok = 0, ko = 0, tm = 0;
    if(!!test.childs) {
        var rs = test.childs.reduce(function (status, test) {
                                        var ts = runTest(test);
                                        return { ok: status.ok + ts.ok
                                               , ko: status.ko + ts.ko
                                               , tm: status.tm + ts.tm
                                               , html: status.html +  ts.html
                                               };
                                    }, {ok: 0, ko: 0, tm: 0, html: ''});
        ok = rs.ok;
        ko = rs.ko;
        tm = rs.tm;
        html = rs.html;
    } else
    if(!!test.test) {
        var t0 = new Date();
        try {
            if(test.test()) ok = 1;
            else           ko = 1;
        } catch(ex) {
            html = toHtml('EXCEPTION: ' + ex);
            ko = 1;
        }
        var t1 = new Date();
        tm = t1.getTime() - t0.getTime();
    }
    return { ok: ok
           , ko: ko
           , tm: tm
           , html: '<div class="test ' + (ko == 0 ? 'ok' : 'ko') + '">' +
                       '<input type="checkbox" />' +
                       '<span class="tm sum">' + (tm * 1e-3).toFixed(1) + ' Sg</span>' +
                       '<span class="ko sum">' + ko + ' ko</span>' +
                       '<span class="ok sum">' + ok + ' ok</span>' +
                       '<h1>' + toHtml(test.title) + '</h1>' +
                        html +
                   '</div>'
           };
}

function runTests(tests) {
    document.getElementById('testResult').innerHTML = tests.map(function(test) { return runTest(test).html }).join('\n');
}














/*
            var allTests = [
                { title: 'Test testing framework'
                    , enabled: true
                    , childs: [
                        { title: 'Show OK message'
                            , enabled: true
                            , test: function () { return true }
                            },
                        { title: 'This test **IS DISABLED**'
                            , enabled: false
                            , test: function () { throw "**DISABLED**" }
                            },
                        { title: 'Show KO message'
                            , enabled: true
                            , test: function () { throw "Argh!" }
                            },
                    ]
                },
                ...
    <body onload="runTests(allTests)">
        <div id="testResult">
        </div>
    </body>
*/
