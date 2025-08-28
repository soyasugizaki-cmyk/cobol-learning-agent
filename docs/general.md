# COBOL仕様概要

## 言語と標準について

* COBOLは **業務アプリケーション向けの手続き型言語** です
* プログラムは4つのDIVISIONで構成されます
  * **IDENTIFICATION DIVISION**: プログラム名などのメタ情報
  * **ENVIRONMENT DIVISION**: 実行環境・入出力デバイス定義
  * **DATA DIVISION**: データ構造（WORKING-STORAGE、FILE SECTIONなど）
  * **PROCEDURE DIVISION**: 実処理の流れを記述
* 代表的なデータ型は **PIC句（PICTURE句）** で表現します（例: `PIC 9(5)V99`）
* ファイル編成は **SEQUENTIAL / INDEXED / RELATIVE** に大別されます

## 開発・実行環境について

* 代表的なコンパイラ/処理系
  * **GnuCOBOL**（オープンソース）
  * **IBM Enterprise COBOL**（メインフレームz/OSで利用）
  * **Micro Focus COBOL**（Windows/UNIXで利用）
* 典型的なビルド・実行（GnuCOBOLの例）
  ```bash
  cobc -x -o hello hello.cob
  ./hello
  ```
* メインフレーム環境では **JCL** を使用してジョブ管理
* 文字コードは **EBCDIC（メインフレーム）/ ASCII（分散環境）** の差異に注意

## ドキュメントについて

* COBOLプロジェクトのドキュメントはMarkdownで管理すると追いやすい
* 推奨構成
  * `docs/overview.md` — 言語・環境の総覧
  * `docs/coding-style.md` — 命名規約、桁あふれ対策、算術精度方針
  * `docs/io-and-files.md` — FILE SECTION、編成別I/O例
  * `docs/testing.md` — テスト戦略、cobol-check等の利用
  * `docs/build-and-deploy.md` — ビルド/デプロイ手順、JCL例

## `cobol-practice-mcp-server` について

COBOL学習を補助するためのMCPサーバー構成例です。

1. `fetch_general` — 演習全体の情報を取得
2. `list_exercises` — 利用可能な演習一覧を取得
3. `get_scaffold` — 雛形コードを取得
4. `run_compile` — コンパイルとビルドログ取得
5. `run_tests` — テスト実行と結果のフィードバック
6. `explain_code` — 文法やコードの解説

## 諸注意

* **桁あふれ**: `ON SIZE ERROR`、`ROUNDED` の使用を徹底
* **数値クラス**: `DISPLAY` / `COMP-3` / `BINARY` の違いを理解
* **固定書式**: Area A/Bに依存するコードは処理系のフリーフォーム対応を確認
* **I/O**: `FILE STATUS` を必ず確認
* **外部仕様**: 処理系・バージョン差で挙動が変わるので公式ドキュメントを確認

---

### 付録：最小サンプル

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLOWORLD.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       PROCEDURE DIVISION.
           DISPLAY "HELLO, COBOL!".
           STOP RUN.
```

### 付録：ファイルI/O例（順編成）

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. READSEQ.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INFILE ASSIGN TO "input.dat"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE  IS SEQUENTIAL
               FILE STATUS  IS WS-FS.
       DATA DIVISION.
       FILE SECTION.
       FD  INFILE
           RECORD CONTAINS 80 CHARACTERS.
       01  IN-REC      PIC X(80).

       WORKING-STORAGE SECTION.
       01  WS-FS       PIC XX.
       01  EOF         PIC X VALUE "N".

       PROCEDURE DIVISION.
           OPEN INPUT INFILE
           PERFORM UNTIL EOF = "Y"
               READ INFILE
                   AT END MOVE "Y" TO EOF
                   NOT AT END DISPLAY IN-REC
               END-READ
           END-PERFORM
           CLOSE INFILE
           GOBACK.
```
