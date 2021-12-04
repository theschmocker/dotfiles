vim.cmd[[
autocmd BufWritePre *.go :silent call CocAction('runCommand', 'editor.action.organizeImport')

augroup myGolang
  au!
  autocmd FileType go
    \  nmap <buffer> <LocalLeader>r   <Plug>(go-run)
    \| nmap <buffer> <LocalLeader>b   <Plug>(go-build)
    \| nmap <buffer> <LocalLeader>t   <Plug>(go-test)
    \| nmap <buffer> <LocalLeader>c   <Plug>(go-coverage)
augroup END
]]
