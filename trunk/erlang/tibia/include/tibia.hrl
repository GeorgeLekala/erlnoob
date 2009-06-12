%%%-------------------------------------------------------------------
%%% File    : tibia.hrl
%%% Author  : Olle Mattsson <olle@zubat>
%%% Description : 
%%%
%%% Created : 15 May 2009 by Olle Mattsson <olle@zubat>
%%%-------------------------------------------------------------------

-define(p, 14299623962416399520070177382898895550795403345466153217470516082934737582776038882967213386204600674145392845853859217990626450972452084065728686565928113).
-define(q, 7630979195970404721891201847792002125535401292779123937207447574596692788513647179235335529307251350570728407373705564708871762033017096809910315212884101).
-define(d, crypto:mpint(46730330223584118622160180015036832148732986808519344675210555262940258739805766860224610646919605860206328024326703361630109888417839241959507572247284807035235569619173792292786907845791904955103601652822519121908367187885509270025388641700821735345222087940578381210879116823013776808975766851829020659073)).
-define(e, crypto:mpint(65537)).
-define(n, crypto:mpint(109120132967399429278860960508995541528237502902798129123468757937266291492576446330739696001110603907230888610072655818825358503429057592827629436413108566029093628212635953836686562675849720620786279431090218017681061521755056710823876476444260558147179707119674283982419152118103759076030616683978566631413)).

-define(UINT, unsigned-integer-little).


-define(LOGIN_PROTOCOL, 16#01).
-define(GAME_PROTOCOL, 16#0A).


-record(state, {server_socket,
		client_socket,
		key,
		account}).
-record(key, {k1,k2,k3,k4}).

-record(tries, {ip,tries}).

-record(node, {type,data,children}).

-record(account, {id,
		  name,
		  password,
		  premdays,
		  lastday,
		  email,
		  key,
		  blocked,
		  warnings,
		  group_id,
		  page_lastday,
		  email_new,
		  email_new_time,
		  created,
		  rlname,
		  location,
		  page_access,
		  email_code,
		  next_email,
		  premium_points}).



-record(player, {id,
		 name,
		 world_id,
		 group_id,
		 account_id,
		 level,
		 vocation,
		 health,
		 healthmax,
		 experience,
		 lookbody,
		 lookfeet,
		 lookhead,
		 looklegs,
		 looktype,
		 lookaddons,
		 maglevel,
		 mana,
		 manamax,
		 manaspent,
		 soul,
		 town_id,
		 posx,
		 posy,
		 posz,
		 conditions,
		 cap,
		 sex,
		 lastlogin,
		 lastip,
		 save,
		 redskull,
		 redskulltime,
		 rank_id,
		 guildnick,
		 lastlogout,
		 blessings,
		 balance,
		 stamina,
		 direction,
		 loss_experience,
		 loss_mana,
		 loss_skills,
		 loss_items,
		 premend,
		 online,
		 marriage,
		 promotion,
		 deleted,
		 created,
		 nick_verify,
		 hide_char,
		 comment}).
