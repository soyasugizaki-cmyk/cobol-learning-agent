# COBOL 学習コンテンツ

このファイルは、COBOL 学習コースの各ステップの**詳細内容**です。  
順番に読み進めることで、基礎から実務応用までの流れを学べます。

---

## 1. COBOLとは
- **概要**: COBOL（Common Business Oriented Language）は、事務処理・会計・銀行システムなどのビジネス用途に最適化された言語です。
- **特徴**
  - 英語に近い可読性の高い構文（DIVISION/SECTION/PARAGRAPHなど）
  - 長年の運用実績と巨大なレガシー資産
  - バッチ処理や定型帳票出力に強い
- **主なユースケース**: 金融勘定系・給与/販売/在庫などの基幹業務

---

## 2. COBOLプログラムの基本構造
COBOL は 4 つの DIVISION で構成されます。最小構成の例：
```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.
       PROCEDURE DIVISION.
           DISPLAY "HELLO, COBOL".
           STOP RUN.
```
- **IDENTIFICATION DIVISION**: プログラム識別情報
- **ENVIRONMENT DIVISION**: 実行環境・入出力定義（ファイル制御等）
- **DATA DIVISION**: 変数・ファイルレコード定義
- **PROCEDURE DIVISION**: 実処理（命令文を順に記述）

> 末尾のピリオド（`.`）は**文や段落の終端**を表します。

---

## 3. DISPLAY文とは
- **概要**: 文字列や変数の内容を出力します。
```cobol
       PROCEDURE DIVISION.
           DISPLAY "HELLO, WORLD.".
           STOP RUN.
```
- **変数の表示例**（複数値を連結して出力できます）
```cobol
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  X-NUM        PIC 9(3) VALUE 123.
       01  MSG          PIC X(20) VALUE "VALUE OF X = ".
       PROCEDURE DIVISION.
           DISPLAY MSG X-NUM.
           STOP RUN.
```
- **WITH NO ADVANCING** を使うと改行せずに表示を続けられます。

---

## 4. 変数の定義
- **数値**: `PIC 9`（桁数分の数字）。符号付きは `S9`。
- **文字列**: `PIC X`（桁数分の任意文字）。
- **初期値**: `VALUE` 句で設定。
```cobol
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  NUM1         PIC 9(3)     VALUE 100.
       01  NUM2         PIC S9(3)    VALUE 200.
       01  NAME         PIC X(10)    VALUE "TARO".
```
- **編集項目（表示整形）**: `PIC ZZ9` などで前ゼロ抑制等が可能。

---

## 5. 四則演算
- 代表的な命令: `ADD`, `SUBTRACT`, `MULTIPLY`, `DIVIDE`、および汎用の `COMPUTE`。
```cobol
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  A            PIC 9(3) VALUE 10.
       01  B            PIC 9(3) VALUE 3.
       01  R            PIC 9(3).
       PROCEDURE DIVISION.
           ADD A TO B.                *> B = B + A
           SUBTRACT 2 FROM B GIVING R.
           MULTIPLY A BY B GIVING R.
           DIVIDE A BY B GIVING R.
           COMPUTE R = (A + B) * 2.
           DISPLAY "RESULT=" R.
           STOP RUN.
```
> `COMPUTE` は複合式を 1 文で記述でき、可読性が高いです。

---

## 6. IF文（条件分岐）
```cobol
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  SCORE        PIC 9(3) VALUE 75.
       PROCEDURE DIVISION.
           IF SCORE >= 80
               DISPLAY "A"
           ELSE IF SCORE >= 70
               DISPLAY "B"
           ELSE
               DISPLAY "C"
           END-IF.
           STOP RUN.
```
- ネストした `IF` は `END-IF` を忘れずに。
- 多分岐は `EVALUATE`（switchに類似）で整理可能。

---

## 7. PERFORM文（繰り返し）
代表的な繰り返しパターン：
- **指定回数**: `PERFORM n TIMES`
- **条件付き**: `PERFORM UNTIL 条件`
- **カウンタ**: `PERFORM VARYING 変数 FROM 初期 BY 増分 UNTIL 条件`
```cobol
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  I            PIC 9(2) VALUE 1.
       PROCEDURE DIVISION.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 5
               DISPLAY I
           END-PERFORM.
           STOP RUN.
```

---

## 8. サブルーチンと段落
- PROCEDURE DIVISION を段落（PARAGRAPH）に分け、`PERFORM 段落名` で呼び出します。
```cobol
       PROCEDURE DIVISION.
           PERFORM INIT-PROC.
           PERFORM MAIN-PROC.
           STOP RUN.

       INIT-PROC.
           DISPLAY "INIT".
           EXIT.

       MAIN-PROC.
           DISPLAY "MAIN".
           EXIT.
```
- 複数段落をまとめて呼ぶ場合は `PERFORM A THRU C.` と記述します。

---

## 9. ファイル入出力（順編成の例）
最小例として**入力ファイルを読み切って表示**するバッチ処理：
```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. READFILE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IN-FILE ASSIGN TO "input.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  IN-FILE.
       01  IN-REC       PIC X(256).

       WORKING-STORAGE SECTION.
       01  EOF-SW       PIC X VALUE "N".
           88  EOF                   VALUE "Y".
           88  NOT-EOF               VALUE "N".

       PROCEDURE DIVISION.
           OPEN INPUT IN-FILE.
           PERFORM UNTIL EOF
               READ IN-FILE
                   AT END     SET EOF     TO TRUE
                   NOT AT END DISPLAY IN-REC
               END-READ
           END-PERFORM.
           CLOSE IN-FILE.
           STOP RUN.
```
- **FILE-CONTROL/FD** でファイルを定義し、`OPEN`→`READ/WRITE`→`CLOSE` の順で操作します。
- `AT END` / `NOT AT END` で終端判定。

---

## 10. 実務での活用と次の一歩
- **活用領域**: バッチ処理（締め・集計・明細生成）、帳票、データ移行 等
- **モダナイゼーション**: API 連携、周辺を他言語化し COBOL をコア計算として残す方針も一般的
- **次に学ぶべき項目**: `EVALUATE`, テーブル（配列）と `OCCURS`, 文字列操作、日時処理、ソート/マージ、エラー処理、ユニットテスト

---

### 付録: ベストプラクティス（抜粋）
- 命名は**意図が明確**になるように（段落名・変数名）
- 文末のピリオドを正しく使い、文のスコープを分かりやすく
- データ定義（`DATA DIVISION`）と処理（`PROCEDURE DIVISION`）の責務分離
- 外部I/O は**エラー/EOF ハンドリング**を必須に
