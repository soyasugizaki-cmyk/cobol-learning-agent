# COBOL Document MCP Server

COBOL言語のドキュメントを参照するためのMCP（Model Context Protocol）サーバーです。

## 概要

このMCPサーバーは、LLM（Large Language Model）がCOBOL言語に関する質問に回答する際に、適切なドキュメントを提供します。COBOLの文法、データ構造、ファイル操作、プログラム構造などについて、包括的な情報を提供し解答の精度を向上させます。

## 機能

### 提供ツール

1. **fetch_general** - COBOLの一般的な情報を取得
2. **list_documents** - 利用可能なドキュメント一覧を取得
3. **search_document** - 特定のドキュメントの内容を検索

### ドキュメントカテゴリー

- **language-basics** - COBOL言語の基礎
- **data-structures** - データ構造について
- **file-operations** - ファイル操作について
- **program-structure** - プログラム構造について

## インストール

```bash
npm install
```

## ビルド

```bash
npm run build
```

## 開発

```bash
npm run dev
```

## 使用方法

このMCPサーバーは、CursorなどのMCP対応エディタで使用できます。

### 設定例

```json
{
  "mcpServers": {
    "cobol-document": {
      "command": "node",
      "args": ["${workspaceFolder}/dist/index.js"],
      "env": {}
    }
  }
}
```

## ドキュメント構成

```
docs/
├── general.md                    # COBOL仕様概要
├── language-basics/             # 言語基礎
│   └── COBOL言語の基礎.md
├── data-structures/             # データ構造
│   └── データ構造について.md
├── file-operations/             # ファイル操作
│   └── ファイル操作について.md
└── program-structure/           # プログラム構造
    └── プログラム構造について.md
```

## 特徴

- **包括的な情報**: COBOL言語の主要な側面をカバー
- **実用的な例**: 実際のコード例とベストプラクティス
- **日本語対応**: 日本語でのドキュメント提供
- **構造化された情報**: カテゴリー別の整理された情報

## ライセンス

MIT License

## 貢献

COBOLに関するドキュメントの改善や追加にご協力ください。
