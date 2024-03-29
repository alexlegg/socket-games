// Generated by CoffeeScript 1.7.1
(function() {
  var displayGame, formatDate;

  formatDate = function(d) {
    var ampm, d_names, h, s;
    d = new Date(d);
    d_names = ["Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"];
    ampm = d.getHours() < 13 ? "am" : "pm";
    h = d.getHours() % 12;
    h = h === 0 ? 12 : h;
    return s = d_names[d.getDay()] + " " + d.getDate() + "/" + (d.getMonth() + 1) + "/" + d.getFullYear() + " " + h + ":" + d.getMinutes() + ampm;
  };

  jQuery(function() {
    var gameReq, gamesReq;
    switch (window.location.pathname) {
      case "/games":
        gamesReq = $.ajax({
          url: '/api?type=games'
        });
        return gamesReq.done(function(res, status, jqXHR) {
          var li, r, _i, _len, _results;
          if (!res) {
            return;
          }
          _results = [];
          for (_i = 0, _len = res.length; _i < _len; _i++) {
            r = res[_i];
            li = $("<a>").addClass("list-group-item").prop("href", "/game?id=" + r.id).html($("<b>").text(formatDate(r.date))).append($("<br>")).append(r.name);
            _results.push($("#gamelist").append(li));
          }
          return _results;
        });
      case "/game":
        gameReq = $.ajax({
          url: '/api' + window.location.search + '&type=game'
        });
        return gameReq.done(function(res, status, jqXHR) {
          if (!res) {
            alert("abort abort!");
          }
          return displayGame(res);
        });
    }
  });

  displayGame = function(game) {
    var a, i, li, m, mi, mv, p, pid, player_vote, player_votes, players, ptr, pv, sc, span, table, target, td, v, voteicon, _fn, _i, _j, _k, _l, _len, _len1, _len2, _len3, _len4, _len5, _len6, _m, _n, _o, _ref, _ref1, _ref2, _ref3, _ref4, _ref5, _results;
    players = [];
    _ref = game.players;
    for (_i = 0, _len = _ref.length; _i < _len; _i++) {
      p = _ref[_i];
      players[p.id] = p;
    }
    if (game.evilWon) {
      $("#gameover").addClass("evil").text("Game Over. The Minions of Mordred win!");
    } else {
      $("#gameover").addClass("good").text("Game Over. The Servants of Arthur win!");
    }
    if (game.assassinated !== void 0) {
      target = players[game.assassinated];
      $("#missionmessage").append("<br />" + target.name + " was assassinated.");
    }
    _ref1 = game.players;
    for (_j = 0, _len1 = _ref1.length; _j < _len1; _j++) {
      p = _ref1[_j];
      li = $('<li>').addClass("list-group-item").text(p.name).append($("<span>")).text(p.role).addClass("role");
      if (p.isEvil) {
        li.addClass("evil");
      } else {
        li.addClass("good");
      }
      $("#players").append(li);
    }
    _ref2 = game.missions;
    _fn = function(table) {
      return a.on('click', function(e) {
        return table.toggle();
      });
    };
    _results = [];
    for (mi = _k = 0, _len2 = _ref2.length; _k < _len2; mi = ++_k) {
      m = _ref2[mi];
      if (m.players.length === 0) {
        continue;
      }
      a = $('<a>').addClass("list-group-item");
      if (m.status === 2) {
        sc = 'good';
      } else if (m.status === 1) {
        sc = 'evil';
      }
      _ref3 = m.players;
      for (i = _l = 0, _len3 = _ref3.length; _l < _len3; i = ++_l) {
        p = _ref3[i];
        span = $('<span>').addClass(sc).text(players[p.id].name);
        if (i !== m.players.length - 1) {
          span.append(', ');
        }
        if (!p.success) {
          span.addClass('bold');
        }
        a.append(span);
      }
      table = $("<table>").addClass("vote-table");
      player_votes = [];
      _ref4 = game.votes;
      for (_m = 0, _len4 = _ref4.length; _m < _len4; _m++) {
        v = _ref4[_m];
        if (v.mission !== mi) {
          continue;
        }
        player_vote = [];
        _ref5 = v.votes;
        for (_n = 0, _len5 = _ref5.length; _n < _len5; _n++) {
          mv = _ref5[_n];
          player_vote[mv.id] = mv.vote;
        }
        player_votes.push(player_vote);
      }

      /*
      tr = $("<tr>")
      tr.append($("<th>").text(""))
      
      for x in [1..player_votes.length]
          tr.append($("<th>").text(x))
      
      table.append(tr)
       */
      for (pid in players) {
        p = players[pid];
        ptr = $("<tr>");
        ptr.append($("<td>").text(p.name));
        for (_o = 0, _len6 = player_votes.length; _o < _len6; _o++) {
          pv = player_votes[_o];
          td = $("<td>").addClass("vote_td");
          voteicon = pv[pid] ? 'tick' : 'cross';
          ptr.append('<img class="icon" src="' + voteicon + '.png" />');
        }
        table.append(ptr);
      }
      a.append(table);
      _fn(table);
      _results.push($("#missions").append(a));
    }
    return _results;
  };

}).call(this);
