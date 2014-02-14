module.exports = {
	dest: 'docs/.build',
	locales: {
		'/': {
			lang: 'zh-CN',
			title: 'Deus',
			description: 'No game no life'
		}
	},
	themeConfig: {
		repo: 'GalAster/Illusory',
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
			}
		]
	},
	serviceWorker: true
};
