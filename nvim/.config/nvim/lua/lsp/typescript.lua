local util = require'lspconfig.util'

local function get_package_json()
	local p_json_dir = util.find_package_json_ancestor(util.path.sanitize(vim.fn.expand('%:p')))
	if p_json_dir == nil then
		return nil
	end

	local p_json_path = p_json_dir .. '/package.json'
    local file = io.open(p_json_path, "r")
    if file then
        local contents = file:read( "*a" )
        local p_json = vim.json.decode(contents);
        io.close( file )
		return p_json
    end

    return nil
end

local function is_vue_project(p_json)
	return p_json ~= nil and p_json.dependencies ~= nil and p_json.dependencies.vue ~= nil
end

local function get_global_node_modules_path()
	-- TODO: is there a better way to call out to npm AND/OR a better way to get the global node_modules dir
	return string.gsub(vim.fn.system('npm root -g'), '\n', '')
end

return {
	-- In Vue projects, I want to use Volar's Takeover mode for JS/TS files. Otherwise, I want to use tsserver.
	-- This will set up the relevant server based on the presence of vue in package.json. There may be a better way to handle this.
	-- @param config table with the `capabilities` and `on_attach` keys to pass to lspconfig's setup
	setup = function (config)
		local default_root_dir = util.root_pattern('package.json')

		-- temporary fix for volar's document version issue
		vim.lsp.util.apply_text_document_edit = function(text_document_edit, index, offset_encoding)
			local text_document = text_document_edit.textDocument
			local bufnr = vim.uri_to_bufnr(text_document.uri)
			if offset_encoding == nil then
				vim.notify_once('apply_text_document_edit must be called with valid offset encoding', vim.log.levels.WARN)
			end

			vim.lsp.util.apply_text_edits(text_document_edit.edits, bufnr, offset_encoding)
		end

		require'lspconfig'.volar.setup{
			capabilities = config.capabilities,
			on_attach = config.on_attach,
			filetypes = {'typescript', 'javascript', 'javascriptreact', 'typescriptreact', 'vue'},
			root_dir = function (filepath)
				if is_vue_project(get_package_json()) then
					return default_root_dir(filepath)
				else
					return nil -- language server will not be enabled
				end
			end
		}
		require'lspconfig'.tsserver.setup{
			capabilities = config.capabilities,
			on_attach = config.on_attach,
			-- TODO: I wonder how I could hook into init_options more dynamically. There doesn't seem to be an issue if the plugin isn't installed, but it'd be interesting to somehow extract
			-- this to a plugin. Maybe one that handled registering the TS plugin automatically in a Svelte workspace, installing it, etc. like the VSCode plugin. VSCode handles TS plugins
			-- in a special way
			init_options = {
				plugins = {
					{
						name = "typescript-svelte-plugin",
						-- TODO: is there a better way to join paths, a la node's path.join?
						location = get_global_node_modules_path() .. '/typescript-svelte-plugin'
					}
				}
			},
			root_dir = function (filepath)
				if not is_vue_project(get_package_json()) then
					return default_root_dir(filepath)
				else
					return nil -- language server will not be enabled
				end
			end
		}
	end
}
