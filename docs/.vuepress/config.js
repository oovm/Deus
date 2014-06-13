module.exports = {
	dest: 'docs/.build',
	locales: {
		'/': {
			lang: 'zh-CN',
			title: 'Deus',
			description: 'No game no life ♕'
		}
	},
	themeConfig: {
		repo: 'GalAster/Deus',
		editLinks: true,
		docsDir: 'docs',
		markdown: {
			lineNumbers: true
		},
		sidebar: [
			{
				title: '开发文档',
				children: [
					'/Start/',
					'/Start/Developer.md',
					'/Start/Editor.md',
					'/Start/EditorAdv.md'
				]
			},
			{
				title: 'MagicSquare 模块',
				children: [
					'/MagicSquare/'
				]
			},
			{
				title: 'NumberMaster 模块',
				children: [
					'/NumberMaster/'
				]
			},
			{
				title: 'HanoiTower 模块',
				children: [
					'/HanoiTower/'
				]
			},
			{
				title: 'Sudoku 模块',
				children: [
					'/Sudoku/',
					'/Sudoku/Object.md'
				]
			}
		]
	},
	serviceWorker: true
};
