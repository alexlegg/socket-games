// Generated by CoffeeScript 1.7.1
(function() {
  var ADMINPW, DEBUG, IP, PORT, select_for_mission,
    __indexOf = [].indexOf || function(item) { for (var i = 0, l = this.length; i < l; i++) { if (i in this && this[i] === item) return i; } return -1; };

  PORT = 3000;

  IP = "localhost";

  DEBUG = true;

  ADMINPW = "adminpassword_changeme";

  jQuery(function() {
    var GAME_ASSASSIN, GAME_FINISHED, GAME_LOBBY, GAME_PREGAME, GAME_PROPOSE, GAME_QUEST, GAME_VOTE, query, socket;
    query = "";
    if ($.cookie('player_id')) {
      query = "?player_id=" + $.cookie('player_id');
    }
    socket = io.connect('http://' + IP + ":" + PORT + query);
    socket.on('connect', function(data) {
        console.log("connect");
        socket.emit("blahblah");
        console.log("blahblah");
      $("#disconnected").hide();
      if (!$.cookie('player_id')) {
        return $("#signin").show();
      }
    });
    socket.on('test response', function() {
        return console.log("test response recvd");
    });
    socket.on('disconnect', function() {
      $("#signin").hide();
      $("#lobby").hide();
      $("#pregame").hide();
      $("#game").hide();
      return $("#disconnected").show();
    });
    GAME_LOBBY = 0;
    GAME_PREGAME = 1;
    GAME_PROPOSE = 2;
    GAME_VOTE = 3;
    GAME_QUEST = 4;
    GAME_ASSASSIN = 5;
    GAME_FINISHED = 6;
    socket.on('player_id', function(player_id) {
      $.cookie('player_id', player_id, {
        expires: 365
      });
      return $("#login").hide();
    });
    socket.on('previous_game', function() {
      return $("#btn_reconnect").show();
    });
    socket.on('bad_login', function() {
      return $("#signin").show();
    });
    socket.on('reconnectlist', function(games) {
      var g, _fn, _i, _len, _results;
      console.log('reconnectlist');
      $("#login").show();
      _fn = function(g) {
        var join_btn;
        join_btn = $('<a>').addClass("list-group-item").text(g.name).append($('<span>').addClass("pull-right").text(g.num_players)).click(function() {
          socket.emit('reconnectuser', {
            game_id: g.id,
            player_id: g.player
          });
          $("#reconnectlist").hide();
          $("#reconnectmessage").text("Waiting for approval to reconnect");
          return $("#reconnectmessage").show();
        });
        return $("#reconnectlist").append(join_btn);
      };
      _results = [];
      for (_i = 0, _len = games.length; _i < _len; _i++) {
        g = games[_i];
        _fn(g);
        $("#reconnectmessage").text("");
        $("#reconnectmessage").hide();
        _results.push($("#reconnectlist").show());
      }
      return _results;
    });
    socket.on('reconnectdenied', function() {
      return $("#reconnectmessage").text("You were rejected");
    });
    socket.on('gamelist', function(games) {
      var g, _i, _len, _results;
      if ($("#pregame").is(":visible")) {
        return;
      }
      if ($("#game").is(":visible")) {
        return;
      }
      $("#lobby").show();
      $("#gamelist").empty();
      _results = [];
      for (_i = 0, _len = games.length; _i < _len; _i++) {
        g = games[_i];
        _results.push((function(g) {
          var join_btn;
          join_btn = $('<a>').addClass("list-group-item").text(g.name).append($('<span>').addClass("pull-right").text(g.num_players)).click(function() {
            return socket.emit('joingame', {
              game_id: g.id
            });
          });
          return $("#gamelist").append(join_btn);
        })(g));
      }
      return _results;
    });
    socket.on('kicked', function() {
      $("#pregame").hide();
      return $("#game").hide();
    });
    socket.on('gameinfo', function(game) {
      var currVote, i, icons, info, input, ishost, lastmission, li, m, me, mission, mission_max, p, player_id, v, votecount, voted, voteicon, _i, _j, _k, _l, _len, _len1, _len2, _len3, _len4, _len5, _len6, _len7, _len8, _m, _n, _o, _p, _q, _ref, _ref1, _ref10, _ref11, _ref12, _ref2, _ref3, _ref4, _ref5, _ref6, _ref7, _ref8, _ref9;
      $("#lobby").hide();
      if (game.state === GAME_LOBBY) {
        $("#pregame").show();
        $("#gameinfo").empty();
        $("#btn_start_game").hide();
        $("#gameoptions").hide();
        ishost = false;
        _ref = game.players;
        for (i = _i = 0, _len = _ref.length; _i < _len; i = ++_i) {
          p = _ref[i];
          li = $('<li>').addClass("list-group-item").text(p.name);
          if (ishost) {
            (function(p) {
              var kick_btn;
              kick_btn = $("<button>").addClass("pull-right").addClass("btn").addClass("btn-danger").addClass("btn-xs").text("Kick").on('click', function(e) {
                return socket.emit('kick', p.id);
              });
              return li.append(kick_btn);
            })(p);
          }
          if (game.me.id === p.id && i === 0) {
            ishost = true;
          }
          $("#gameinfo").append(li);
        }
        if (ishost) {
          $("#btn_ready").show();
        } else {
          $("#btn_ready").hide();
        }
        return window.have_game_info = false;
      } else if (game.state === GAME_FINISHED) {
        socket.emit('leavegame');
        return window.location = '/game?id=' + game.id;

        /*
        $("#game").show()
        
         *Draw mission info
        lastmission = undefined
        for m, i in game.missions
            $("#mission" + i).text(m.numReq)
            if m.failsReq == 2
                $("#mission" + i).append("*")
            if m.status == 1
                lastmission = m
                $("#mission" + i).addClass("evil")
            else if m.status == 2
                lastmission = m
                $("#mission" + i).addClass("good")
        
        if game.evilWon
            $("#missionmessage")
                .removeClass("good")
                .addClass("evil")
                .text("Game Over. The Minions of Mordred win!")
        else
            $("#missionmessage")
                .removeClass("evil")
                .addClass("good")
                .text("Game Over. The Servants of Arthur win!")
        
        if game.assassinated != undefined
            $("#missionmessage").append("<br />" + game.assassinated + " was assassinated.")
         */
      } else if (game.state === GAME_PREGAME) {
        $("#pregame").show();
        $("#btn_ready").hide();
        $("#btn_leavelobby").hide();
        $("#btn_start_game").hide();
        $("#gameoptions").hide();
        if (window.have_game_info === true) {
          return;
        }
        $("#gameinfo").empty();
        ishost = false;
        _ref1 = game.players;
        for (i = _j = 0, _len1 = _ref1.length; _j < _len1; i = ++_j) {
          p = _ref1[i];
          player_id = $("<input>").attr("type", "hidden").attr("value", p.id);
          li = $('<li>').addClass("list-group-item").text(p.name).attr("id", "player" + i).append(player_id);
          $("#gameinfo").append(li);
          if (game.me.id === p.id && i === 0) {
            ishost = true;
            $("#btn_start_game").show();
            $("#gameoptions").show();
            $("#gameinfo").sortable({
              items: "li:not(:first)"
            });
          }
        }
        if (!ishost) {
          $("#waitforhost").show();
        }
        return window.have_game_info = true;
      } else {
        $("#pregame").hide();
        $("#game").show();
        lastmission = void 0;
        _ref2 = game.missions;
        for (i = _k = 0, _len2 = _ref2.length; _k < _len2; i = ++_k) {
          m = _ref2[i];
          $("#mission" + i).text(m.numReq);
          if (m.failsReq === 2) {
            $("#mission" + i).append("*");
          }
          if (m.status === 1) {
            lastmission = m;
            $("#mission" + i).addClass("evil");
          } else if (m.status === 2) {
            lastmission = m;
            $("#mission" + i).addClass("good");
          } else {
            $("#mission" + i).removeClass("good");
            $("#mission" + i).removeClass("evil");
          }
        }
        if (game.state === GAME_PROPOSE) {
          $("#missionmessage").removeClass("good").removeClass("evil");
          votecount = 0;
          _ref3 = game.votes;
          for (_l = 0, _len3 = _ref3.length; _l < _len3; _l++) {
            v = _ref3[_l];
            if (v.mission === game.currentMission) {
              votecount += 1;
            }
          }
          if (votecount > 0) {
            $("#missionmessage").text("Failed voting rounds: " + votecount);
          } else if (lastmission !== void 0) {
            if (lastmission.status === 1) {
              $("#missionmessage").addClass("evil").text("Mission failed! It was probably Dan.");
              if (game.options.showfails) {
                if (lastmission.numfails === 1) {
                  $("#missionmessage").append("<br />There was only 1 fail.");
                } else {
                  $("#missionmessage").append("<br />There were " + lastmission.numfails + " fails.");
                }
              }
            } else {
              $("#missionmessage").addClass("good").text("Mission succeeded!");
            }
          } else {
            $("#missionmessage").text("");
          }
        }
        if (game.state === GAME_QUEST) {
          $("#missionmessage").removeClass("good").removeClass("evil").text("Mission is underway...");
        }
        if (game.state === GAME_ASSASSIN) {
          $("#missionmessage").removeClass("good").removeClass("evil").text("The Assassin can now try to kill Merlin.");
        }
        me = game.me;
        $("#players").empty();
        _ref4 = game.players;
        for (_m = 0, _len4 = _ref4.length; _m < _len4; _m++) {
          p = _ref4[_m];
          li = $("<li>").addClass("list-group-item").text(p.name);
          icons = $("<span>").addClass("pull-right").attr({
            style: "font-size: 16px;"
          });
          if (game.currentLeader === p.id) {
            icons.append('<img class="icon" src="crown.png" />');
          }
          if (game.finalLeader === p.id) {
            icons.append('<img class="icon" src="crown-last.png" />');
          }
          if (game.state === GAME_VOTE || game.state === GAME_QUEST) {
            currVote = game.votes[game.votes.length - 1];
            if (_ref5 = p.id, __indexOf.call(currVote.team, _ref5) >= 0) {
              li.addClass("success");
            }
            voted = [];
            if (currVote.votes) {
              _ref6 = currVote.votes;
              for (_n = 0, _len5 = _ref6.length; _n < _len5; _n++) {
                v = _ref6[_n];
                voted.push(v.id);
              }
            }
            if (!(_ref7 = p.id, __indexOf.call(voted, _ref7) >= 0)) {
              icons.append('<img class="icon" src="clock.png" />');
            }
          }
          if (game.state === GAME_PROPOSE || game.state === GAME_QUEST) {
            currVote = game.votes[game.votes.length - 1];
            if (currVote && currVote.mission === game.currentMission) {
              _ref8 = currVote.votes;
              for (_o = 0, _len6 = _ref8.length; _o < _len6; _o++) {
                v = _ref8[_o];
                if (p.id === v.id) {
                  voteicon = v.vote ? 'tick' : 'cross';
                  icons.append('<img class="icon" src="' + voteicon + '.png" />');
                }
              }
            }
          }
          if (game.currentLeader === me.id) {
            mission_max = 0;
            if (game.state === GAME_PROPOSE) {
              mission = game.missions[game.currentMission];
              mission_max = mission.numReq;
            } else if (game.state === GAME_ASSASSIN) {
              mission_max = 1;
            }
            window.mission_max = mission_max;
            li.on('click', function(e) {
              return select_for_mission(mission_max, $(e.target));
            });
            input = $("<input>").attr({
              type: 'hidden',
              name: p.id,
              value: 0
            });
            li.append(input);
          }
          li.append(icons);
          $("#players").append(li);
        }
        if (game.state === GAME_PROPOSE && game.currentLeader === me.id) {
          $("#btn_select_mission").show();
          $("#leaderinfo").show();
        } else {
          $("#btn_select_mission").hide();
          $("#leaderinfo").hide();
        }
        if (game.state === GAME_ASSASSIN && game.currentLeader === me.id) {
          $("#btn_assassinate").show();
        } else {
          $("#btn_assassinate").hide();
        }
        if (game.state === GAME_VOTE) {
          currVote = game.votes[game.votes.length - 1];
          voted = [];
          if (currVote.votes) {
            _ref9 = currVote.votes;
            for (_p = 0, _len7 = _ref9.length; _p < _len7; _p++) {
              v = _ref9[_p];
              voted.push(v.id);
            }
          }
          if (!(_ref10 = me.id, __indexOf.call(voted, _ref10) >= 0)) {
            $("#vote").show();
          } else {
            $("#vote").hide();
          }
        } else {
          $("#vote").hide();
        }
        if (game.state === GAME_QUEST) {
          currVote = game.votes[game.votes.length - 1];
          if (_ref11 = me.id, __indexOf.call(currVote.team, _ref11) >= 0) {
            $("#quest").show();
          } else {
            $("#quest").hide();
          }
        } else {
          $("#quest").hide();
        }
        $("#hiddeninfo").empty();
        info = $("<span>").text(me.role);
        if (me.isEvil) {
          info.addClass("evil");
        } else {
          info.addClass("good");
        }
        li = $("<li>").addClass("list-group-item").text("You are ").append(info);
        $("#hiddeninfo").append(li);
        _ref12 = me.info;
        for (_q = 0, _len8 = _ref12.length; _q < _len8; _q++) {
          i = _ref12[_q];
          info = $("<span>").text(i.information);
          if (i.information === "evil") {
            info.addClass("evil");
          }
          li = $("<li>").addClass("list-group-item").text(i.otherPlayer + " is ").append(info);
          $("#hiddeninfo").append(li);
        }
        if (game.reconnect_user && game.reconnect_user !== "") {
          if (game.reconnect_vote[me.order] === 0) {
            $("#user_reconnecting_name").text(game.reconnect_user);
            return $("#user_reconnecting").show();
          }
        } else {
          $("#user_reconnecting").hide();
          return $("#user_reconnecting .btn").each(function() {
            return $(this).removeClass("active");
          });
        }
      }
    });
    $("#form-signin").on('submit', function(e) {
      if ($("#playername").val().length > 0) {
        socket.emit('login', {
          name: $("#playername").val()
        });
        $("#signin").hide();
      }
      console.log("submit login");
      return e.preventDefault();
    });
    $("#btn_newgame").on('click', function() {
      return socket.emit('newgame');
    });
    $("#btn_changename").on('click', function() {
      return $("#signin").show();
    });
    $("#btn_reconnect").on('click', function() {
      return socket.emit('reconnecttogame');
    });
    $("#btn_ready").on('click', function() {
      return socket.emit('ready');
    });
    $("#btn_start_game").on('click', function() {
      var i, input, options, p, player_id, players, sorted, _i, _len;
      players = $("#gameinfo").sortable("toArray");
      sorted = {};
      for (i = _i = 0, _len = players.length; _i < _len; i = ++_i) {
        p = players[i];
        input = $("#" + p + " input")[0];
        player_id = $(input).attr("value");
        sorted[player_id] = i + 1;
      }
      options = {};
      options['mordred'] = $("#opt_mordred").is(":checked");
      options['oberon'] = $("#opt_oberon").is(":checked");
      options['showfails'] = $("#opt_showfails").is(":checked");
      return socket.emit('startgame', {
        order: sorted,
        options: options
      });
    });
    $("#btn_showinfo").on('click', function() {
      if ($("#hiddeninfo").is(":visible")) {
        $("#btn_showinfo").text("Show Hidden Info");
      } else {
        $("#btn_showinfo").text("Hide Hidden Info");
      }
      return $("#hiddeninfo").toggle();
    });
    $(".options-list li").on('click', function(e) {
      var chk;
      e.preventDefault();
      chk = $(this).find("input").first();
      if (chk) {
        return chk.prop('checked', !chk.prop('checked'));
      }
    });
    $(".options-list input").on('click', function(e) {
      return e.stopPropagation();
    });
    $("#form-select-mission").on('submit', function(e) {
      var assassinate, mission, s, sel, _i, _len;
      e.preventDefault();
      mission = [];
      sel = [];
      $("#players li").each(function() {
        var input;
        input = $($(this).children(":input")[0]);
        if (input.val() === '1') {
          mission.push(input.attr('name'));
          return sel.push($(this));
        }
      });
      if (mission.length === window.mission_max) {
        assassinate = $("#btn_assassinate").is(":visible");
        $("#btn_select_mission").hide();
        $("#btn_assassinate").hide();
        for (_i = 0, _len = sel.length; _i < _len; _i++) {
          s = sel[_i];
          s.removeClass('active');
          s.addClass('success');
        }
        $("#leaderinfo").html("You are the leader, select players from the list then press this button.");
        if (assassinate) {
          return socket.emit('assassinate', mission[0]);
        } else {
          return socket.emit('propose_mission', mission);
        }
      } else {
        return $("#leaderinfo").html("You must select only <b>" + window.mission_max + "</b> players for the quest!");
      }
    });
    $("#btn_submitvote").on('click', function(e) {
      var radio, vote;
      radio = $("input[name=vote]:checked").val();
      if (radio !== "approve" && radio !== "reject") {
        return;
      }
      vote = radio === "approve";
      $("input[name=vote]:checked").prop('checked', false);
      $("#vote .btn").each(function() {
        return $(this).removeClass("active");
      });
      $("#vote").hide();
      return socket.emit('vote', vote);
    });
    $("#btn_submitquest").on('click', function(e) {
      var quest_card, radio;
      radio = $("input[name=quest]:checked").val();
      if (radio !== "success" && radio !== "fail") {
        return;
      }
      quest_card = radio === "success";
      $("input[name=quest]:checked").prop('checked', false);
      $("#quest .btn").each(function() {
        return $(this).removeClass("active");
      });
      $("#quest").hide();
      return socket.emit('quest', quest_card);
    });
    $("#btn_quit").on('click', function(e) {
      $("#game").hide();
      $("#btn_reconnect").show();
      return socket.emit('leavegame');
    });
    $("#btn_leavelobby").on('click', function(e) {
      $("#pregame").hide();
      return socket.emit('leavegame');
    });
    $("#btn_submitreconnectvote").on('click', function(e) {
      var radio, rvote;
      radio = $("input[name=reconnectvote]:checked").val();
      if (radio !== "allow" && radio !== "deny") {
        return;
      }
      rvote = radio === "allow";
      $("input[name=reconnectvote]:checked").prop('checked', false);
      $("#user_reconnecting .btn").each(function() {
        return $(this).removeClass("active");
      });
      $("#user_reconnecting").hide();
      return socket.emit('reconnect_vote', rvote);
    });
    return $("#btn_noreconnect").on('click', function(e) {
      socket.emit('noreconnect', {
        name: $("#playername").val()
      });
      return $("#login").hide();
    });
  });

  select_for_mission = function(mission_max, li) {
    var assassinating, input, mission_count, proposing;
    proposing = $("#btn_select_mission").is(":visible");
    assassinating = $("#btn_assassinate").is(":visible");
    if (proposing || assassinating) {
      mission_count = 0;
      $("#players li").each(function() {
        var input;
        input = $($(this).children(":input")[0]);
        if (assassinating) {
          $(this).removeClass("active");
          return $(this).attr({
            value: 1
          });
        } else {
          if (input.val() === '1') {
            return mission_count += 1;
          }
        }
      });
      input = $(li.children(":input")[0]);
      if (li.hasClass("active")) {
        li.removeClass("active");
        return input.attr({
          value: 0
        });
      } else {
        if (mission_count + 1 <= mission_max) {
          li.addClass("active");
          return input.attr({
            value: 1
          });
        } else {
          return $("#leaderinfo").html("You must select only <b>" + window.mission_max + "</b> players for the quest!");
        }
      }
    }
  };

}).call(this);
