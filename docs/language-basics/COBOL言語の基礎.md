# COBOL言語の基礎

## 概要

COBOL（Common Business-Oriented Language）は、1959年に開発された業務アプリケーション向けのプログラミング言語です。銀行、保険、政府機関などの大規模な業務システムで広く使用されています。

## 言語の特徴

### 1. 読みやすさ
- 英語に近い自然言語的な構文
- 自己文書化コードの実現
- 保守性の高さ

### 2. 業務処理に特化
- 大量データの処理
- ファイル操作の充実
- 数値計算の精度

### 3. 標準化
- ANSI標準による規格化
- 処理系間の互換性
- 長期的なサポート

## 基本的なプログラム構造

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLOWORLD.
       
       ENVIRONMENT DIVISION.
       
       DATA DIVISION.
       
       PROCEDURE DIVISION.
           DISPLAY "Hello, COBOL World!".
           STOP RUN.
```

## 4つのDIVISION

### IDENTIFICATION DIVISION
- プログラム名、作成者、作成日などの識別情報
- 必須項目：PROGRAM-ID

### ENVIRONMENT DIVISION
- 実行環境の定義
- 入出力デバイスの指定
- ファイルの物理的な配置

### DATA DIVISION
- データの定義と構造
- WORKING-STORAGE SECTION
- FILE SECTION
- LINKAGE SECTION

### PROCEDURE DIVISION
- 実際の処理ロジック
- 手続きの記述
- プログラムのメイン部分

## 基本的な文法

### 文の構造
- 各文はピリオド（.）で終了
- 固定書式（Area A: 1-6行目、Area B: 8-72行目）
- 自由書式も対応（処理系による）

### 命名規則
- 最大30文字
- 英数字とハイフン（-）が使用可能
- 数字で開始不可

### コメント
```cobol
      * これはコメント行です
      * 行全体がコメントになります
```

## データ型

### 数値型
- `PIC 9` - 数字
- `PIC X` - 文字
- `PIC V` - 小数点位置
- `PIC S` - 符号付き

### 例
```cobol
       01  WS-AMOUNT    PIC 9(7)V99.
       01  WS-NAME      PIC X(30).
       01  WS-FLAG      PIC X.
```

## 制御構造

### IF文
```cobol
           IF WS-AMOUNT > 1000
               DISPLAY "高額です"
           ELSE
               DISPLAY "通常額です"
           END-IF.
```

### PERFORM文
```cobol
           PERFORM UNTIL WS-EOF = "Y"
               READ INPUT-FILE
                   AT END MOVE "Y" TO WS-EOF
                   NOT AT END PERFORM PROCESS-RECORD
               END-READ
           END-PERFORM.
```

## ファイル操作

### 基本的なファイル操作
```cobol
           OPEN INPUT INPUT-FILE
           READ INPUT-FILE
           CLOSE INPUT-FILE
```

### ファイル編成
- SEQUENTIAL（順編成）
- INDEXED（索引編成）
- RELATIVE（相対編成）

## エラーハンドリング

### ファイルステータス
```cobol
       01  WS-FILE-STATUS PIC XX.
       
           READ INPUT-FILE
           IF WS-FILE-STATUS NOT = "00"
               DISPLAY "ファイルエラー: " WS-FILE-STATUS
           END-IF
```

### 数値エラー
```cobol
           COMPUTE WS-RESULT = WS-A / WS-B
               ON SIZE ERROR
                   DISPLAY "桁あふれエラー"
               ON ZERO DIVIDE
                   DISPLAY "ゼロ除算エラー"
           END-COMPUTE
```

## 開発環境

### 代表的なコンパイラ
- GnuCOBOL（オープンソース）
- IBM Enterprise COBOL
- Micro Focus COBOL

### ビルドと実行
```bash
# コンパイル
cobc -x -o program program.cob

# 実行
./program
```

## ベストプラクティス

### 1. 命名規則の統一
- 意味のある名前を使用
- 接頭辞・接尾辞の規則化
- 一貫性の維持

### 2. エラーハンドリング
- ファイルステータスの確認
- 数値計算のエラー処理
- 適切なエラーメッセージ

### 3. コメントの充実
- 複雑な処理の説明
- ビジネスロジックの説明
- 変更履歴の記録

### 4. テスト
- 単体テストの実施
- 境界値のテスト
- エラーケースのテスト

## まとめ

COBOLは、長い歴史を持つ堅牢なプログラミング言語です。適切な設計と実装により、保守性の高い業務システムを構築できます。現代的な開発手法と組み合わせることで、より効率的な開発が可能になります。
