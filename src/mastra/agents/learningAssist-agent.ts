import { Agent } from '@mastra/core/agent';
import { openai } from '@ai-sdk/openai';
import { Memory } from '@mastra/memory';
import { LibSQLStore, LibSQLVector } from '@mastra/libsql';
import { MCPClient } from '@mastra/mcp';
import path from 'path';

// MCPサーバーの設定
const mcp = new MCPClient({
    servers: {
      // MCPサーバの識別名を指定
      textEditor: {
        // MCPサーバを起動するためのコマンド
        command: "npx",
        args: [
          // MCPサーバのパッケージを指定。ファイルシステムの操作ができる
          `@modelcontextprotocol/server-filesystem`,
          // fileSystemがいじることのできるディレクトリを指定
          path.join(process.cwd(), "..", "..", "notes"), 
        ],
      },
    },
  });

const memory = new Memory({
  storage: new LibSQLStore({ url: "file:../memory.db" }),
  options: { lastMessages: 20 }
});

const mcpTools = await mcp.getTools();

export const learningAssistAgent = new Agent({
  name: 'Learning Assist Agent',
  instructions: `
  あなたは優秀な学習アシスタントです。
  ユーザーが、「Start cobol course」といった場合、ディレクトリにあるファイルを参照して、学習コースを始めて下さい。
  `,
  model: openai('gpt-4o-mini'),
  tools: { ...mcpTools },
  memory: memory,
});
