
CREATE SCHEMA DOUBUTSU;

-- Rails っぽい命名規則を採用
-- 整数値 primary key -- id
-- foreign key 参照 -- xxx_id
-- 時刻 -- xxx_at -- e.g. updated_at, created_at
-- 日付 -- xxx_on

-- foreign key 制約を利用するのは backup を考えたときの問題が無いと確認できてからにする

-- * ユーザー
CREATE TABLE DOUBUTSU.gameuser -- user が SQL の keyword らしいので避けた
  ( id INTEGER NOT NULL
  , game_id INTEGER NOT NULL
  , email VARCHAR(256) NOT NULL
  , username TEXT NOT NULL
  , created_at TIMESTAMP NOT NULL

  , PRIMARY KEY (id)
  );

CREATE TABLE DOUBUTSU.gameuser_seq
  ( seq INTEGER NOT NULL
  );

INSERT INTO DOUBUTSU.gameuser_seq VALUES (0);

-- * ゲーム
CREATE TABLE DOUBUTSU.game
  ( id INTEGER NOT NULL
  , gameuser_id INTEGER NOT NULL

  , PRIMARY KEY (id)
  );

CREATE TABLE DOUBUTSU.game_seq
  ( seq INTEGER NOT NULL
  );

INSERT INTO DOUBUTSU.game_seq VALUES (0);

-- * 温泉 (ステージ?) - 属性
CREATE TABLE DOUBUTSU.onsen
  ( id SMALLINT NOT NULL
  , onsen_name TEXT NOT NULL
  , released_at TIMESTAMP NOT NULL

  , PRIMARY KEY (id)
  );

CREATE TABLE DOUBUTSU.onsen_seq
  ( seq SMALLINT NOT NULL
  );

INSERT INTO DOUBUTSU.onsen_seq VALUES (0);

-- * スロット (動物が温泉に入ったり、カスタマイズ用の木の種や実を植えたりする) - 属性
CREATE TABLE DOUBUTSU.slot
  ( onsen_id SMALLINT NOT NULL
  , local_slot_number INTEGER NOT NULL
  , locate_x INTEGER NOT NULL
  , locate_y INTEGER NOT NULL
  , slot_type SMALLINT NOT NULL  -- 動物が温泉に入る: 0, カスタマイズ: 1

  , PRIMARY KEY (onsen_id, local_slot_number)
  );

-- * 動物の種類(個体) - 属性
CREATE TABLE DOUBUTSU.doubutsu
  ( id SMALLINT NOT NULL
  , doubutsu_name TEXT NOT NULL
  , released_at TIMESTAMP NOT NULL

  , PRIMARY KEY (id)
  );

CREATE TABLE DOUBUTSU.doubutsu_seq
  ( seq SMALLINT NOT NULL
  );

INSERT INTO DOUBUTSU.doubutsu_seq VALUES (0);

-- * 動物のスロット占有相対座標
CREATE TABLE DOUBUTSU.doubutsu_size
  ( doubutsu_id SMALLINT NOT NULL
  , local_coord_number INTEGER NOT NULL
  , relative_x INTEGER NOT NULL
  , relative_y INTEGER NOT NULL

  , PRIMARY KEY (doubutsu_id, local_coord_number)
  );

-- * 種や木の実の種類 - 属性
CREATE TABLE DOUBUTSU.item
  ( id SMALLINT NOT NULL
  , item_name TEXT NOT NULL
  , rarity SMALLINT NOT NULL  -- 通常:0 , レア:1 , 激レア:2

  , PRIMARY KEY (id)
  );

CREATE TABLE DOUBUTSU.item_seq
  ( seq SMALLINT NOT NULL
  );

INSERT INTO DOUBUTSU.item_seq VALUES (0);

-- * 種や木の実の所持状態
CREATE TABLE DOUBUTSU.item_stock
  ( game_id INTEGER NOT NULL
  , item_id SMALLINT NOT NULL
  , amount SMALLINT NOT NULL

  , PRIMARY KEY (game_id, item_id)
  );

-- * 種や木の実をもらった理由詳細

-- ** 動物が帰ったときにもらう - 属性
-- got_reason_type == 0
-- got_reason_data == <動物のID>

-- ** その他


-- * ミッション (温泉レベルアップ条件) - 属性
CREATE TABLE DOUBUTSU.mission
  ( onsen_id SMALLINT NOT NULL
  , onsen_level SMALLINT NOT NULL
  , mission_type SMALLINT NOT NULL

  , PRIMARY KEY (onsen_id, onsen_level)
  );

-- * 各ミッション詳細
-- ** はじめてのお客さん - 属性
-- mission_type == 0
CREATE TABLE DOUBUTSU.mission_first_visited
  ( onsen_id SMALLINT NOT NULL
  , onsen_level SMALLINT NOT NULL
  , doubutsu_id SMALLINT NOT NULL

  , PRIMARY KEY (onsen_id, onsen_level)
  );

-- ** 特定の動物がn回、来た - 属性
-- mission_type == 1
CREATE TABLE DOUBUTSU.mission_visited
  ( onsen_id SMALLINT NOT NULL
  , onsen_level SMALLINT NOT NULL
  , doubutsu_id SMALLINT NOT NULL
  , times SMALLINT NOT NULL

  , PRIMARY KEY (onsen_id, onsen_level)
  );

-- ** 特定の種あるいは木の実をn回、植えた - 属性
-- mission_type == 2
CREATE TABLE DOUBUTSU.mission_used_item
  ( onsen_id SMALLINT NOT NULL
  , onsen_level SMALLINT NOT NULL
  , item_id SMALLINT NOT NULL
  , times SMALLINT NOT NULL

  , PRIMARY KEY (onsen_id, onsen_level)
  );

-- ** どうぶつのお悩み - 属性
-- mission_type == 3


-- * イベントログ
CREATE TABLE DOUBUTSU.event_log
  ( id BIGINT NOT NULL
  , game_id INTEGER NOT NULL
  , onsen_id SMALLINT NOT NULL
  , event_type SMALLINT NOT NULL
  , created_at TIMESTAMP NOT NULL

  , PRIMARY KEY (id)
  );

CREATE TABLE DOUBUTSU.event_log_seq
  ( seq BIGINT NOT NULL
  );

INSERT INTO DOUBUTSU.event_log_seq VALUES (0);

-- * 各イベント詳細
-- ** 温泉のはじまり
--  event_type == 0

-- ** 動物の行動
--  event_type == 1
--  event_data == id
CREATE TABLE DOUBUTSU.event_action
  ( event_log_id BIGINT NOT NULL
  , local_slot_number INTEGER NOT NULL
  , doubutsu_id SMALLINT NOT NULL
  , action_type SMALLINT NOT NULL -- 来る: 0 , 帰る: 1 , or 癒しポーズID

  , PRIMARY KEY (event_log_id)
  );

-- ** 種や木の実をもらう - 属性
--  event_type == 2
--  event_data == id
CREATE TABLE DOUBUTSU.event_item
  ( event_log_id BIGINT NOT NULL
  , item_id SMALLINT NOT NULL
  , got_reason_type SMALLINT NOT NULL
  , got_reason_data INTEGER NOT NULL

  , PRIMARY KEY (event_log_id)
  );

-- ** 種や木の実の成長
--  event_type == 3
--  event_data == id
CREATE TABLE DOUBUTSU.event_growth
  ( event_log_id BIGINT NOT NULL
  , local_slot_number INTEGER NOT NULL
  , item_id SMALLINT NOT NULL
  , growth_level INTEGER NOT NULL -- 植えたときはレベル1

  , PRIMARY KEY (event_log_id)
  );

-- ** ミッション(の状態遷移)
--  event_type == 4
--  event_data == id
CREATE TABLE DOUBUTSU.event_mission
  ( event_log_id BIGINT NOT NULL
  , onsen_level SMALLINT NOT NULL
  , mission_status INTEGER NOT NULL -- 遷移後状態

  , PRIMARY KEY (event_log_id)
  );

-- ** 時間が経つ
--  event_type == 5
CREATE TABLE DOUBUTSU.event_time
  ( event_log_id BIGINT NOT NULL
  , time_type SMALLINT NOT NULL -- 朝: 0 , 昼: 1 , 夕方: 2 , 夜: 3
  );

-- ** 天気が変わる
--  event_type == 6
CREATE TABLE DOUBUTSU.event_whether
  ( event_log_id BIGINT NOT NULL
  , whether SMALLINT NOT NULL -- 天気の状態のID
  );


-- * 温泉の状態 - 属性
CREATE TABLE DOUBUTSU.onsen_status
  ( game_id  INTEGER NOT NULL
  , onsen_id SMALLINT NOT NULL
  , updated_at TIMESTAMP NOT NULL
  , onsen_level SMALLINT NOT NULL
  , mission_status INTEGER NOT NULL  -- 未開始: 0 , 実行中: 1 , 達成: 2
  , seed INTEGER NOT NULL
  , started_at TIMESTAMP NOT NULL

  , PRIMARY KEY (game_id , onsen_id)
  );

-- * スロットの状態詳細
-- ** 動物が入る - 属性
-- slot_type == 0
CREATE TABLE DOUBUTSU.slot_status_doubutsu
  ( game_id  INTEGER NOT NULL
  , onsen_id SMALLINT NOT NULL
  , local_slot_number INTEGER NOT NULL
  , doubutsu_id SMALLINT NOT NULL -- 入っていない: -1
  -- , doubutsu_pause INTEGER NOT NULL

  , PRIMARY KEY (game_id, onsen_id, local_slot_number)
  );

-- ** カスタマイズ - 属性
-- slot_type == 1
CREATE TABLE DOUBUTSU.slot_status_custom
  ( game_id  INTEGER NOT NULL
  , onsen_id SMALLINT NOT NULL
  , local_slot_number INTEGER NOT NULL
  , item_id SMALLINT NOT NULL -- 何も植えられていない: -1
  , growth_level INTEGER NOT NULL

  , PRIMARY KEY (game_id, onsen_id, local_slot_number)
  );
