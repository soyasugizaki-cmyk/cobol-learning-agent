import { createTool } from '@mastra/core/tools';
import { z } from 'zod';
import fs from 'fs/promises';
import path from 'path';

export const getMaxDoneStep = createTool({
    id: 'getMaxDoneStep',
    description: 'notes/done.txt に記載されている最大のstepを取得する',
    inputSchema: z.object({}),
    outputSchema: z.object({
        maxDoneStepNumber: z.number(),
    }),
    execute: async () => {
        // done.txtを読み込む
        const notesDir = path.join(process.cwd(), "..", "..", "notes");
        const donePath = path.join(notesDir, 'done.txt');
        const doneContent = await fs.readFile(donePath, 'utf-8');
        // ステップ番号を取得
        // 改行ごとに分割し、配列に格納[1. aaa, 2. bbb, 3. ccc, ...]
        const completedSteps = doneContent.split('\n').filter(line => line.trim());
        // ステップ番号を取得
        // reduceメソッド: maxの部分に返り値、lineの部分で配列の前から要素を取り出す。
        const maxStep = completedSteps.reduce((max, line) => {
            const match = line.match(/^(\d+)\./);  // 正規表現で先頭の数字を取り出す
            return match ? Math.max(max, parseInt(match[1])) : max; // 数字の最大値を取得 // parseIntは文字列を数値に変換する // 条件 ? 真の場合 : 偽の場合
          }, 0); // この0はmax部分の初期値。

        return {
            maxDoneStepNumber: maxStep,
        }
    }
})