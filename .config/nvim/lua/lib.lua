local M = {}

function table.merge(a, b)
    if type(a) == 'table' and type(b) == 'table' then
        for k,v in pairs(b) do if type(v)=='table' and type(a[k] or false)=='table' then merge(a[k],v) else a[k]=v end end
    end
    return a
end


function M.nmap(...)
    vim.api.nvim_set_key_map('n',...)
end
function M.map(...)
    vim.api('',...)
end
function M.imap(...)
    vim.api.nvim_set_keymap('i',...)
end

function M.inoremap(a,bind,command)
    vim.api.nvim_set_keymap('i',bind,command,merge({noremap = true },a))
end

return M
