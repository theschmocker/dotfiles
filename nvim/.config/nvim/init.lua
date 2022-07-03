local execute = vim.api.nvim_command
local fn = vim.fn

local pack_path = fn.stdpath("data") .. "/site/pack"
local fmt = string.format

local function ensure(user, repo, after_install)
	-- Ensures a given github.com/USER/REPO is cloned in the pack/packer/start directory.
	local install_path = fmt("%s/packer/start/%s", pack_path, repo)
	if fn.empty(fn.glob(install_path)) > 0 then
		fn.system({
			"git",
			"clone",
			"--depth",
			"1",
			fmt("https://github.com/%s/%s", user, repo),
			install_path,
		})
		execute(fmt("packadd %s", repo))
		if after_install then
			after_install()
		end
	end
end

-- Bootstrap essential plugins required for installing and loading the rest.
ensure("wbthomason", "packer.nvim", function ()
	require('packer').sync()
end)
ensure("Olical", "aniseed")

vim.g["aniseed#env"] = {
	module = "user.init",
	compile = true,
}

-- require('user.plugins')
-- require('old_lsp')
