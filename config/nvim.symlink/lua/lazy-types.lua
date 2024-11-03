--- lazy.nvim, a modern plugin manager for Neovim
-- @author Folke Lemaitre and contributors @ https://github.com/folke/lazy.nvim
---
--- This is the annotations file for Lazy types used by `lua-language-server`
--- matching the one called `types.lua` potentially found in your directory tree
--- at `~/.local/share/nvim/lazy/lazy.nvim/lua/lazy` and augmented by
--- https://gitlab.com/martialhb to fix some missing classes

---@alias AsyncEvent "done" | "error" | "yield" | "ok"

---@class Async
---@field _co thread
---@field _fn fun()
---@field _suspend? boolean
---@field _on table<AsyncEvent, fun(res:any, async:Async)[]>

---@class LazyMsg
---@field msg string
---@field level? number

---@class LazyTask: Async
---@field plugin LazyPlugin
---@field name string
---@field private _log LazyMsg[]
---@field private _started number
---@field private _ended? number
---@field private _opts number
---@field private _level number

---@class Semver
---@field [1] number
---@field [2] number
---@field [3] number
---@field major number
---@field minor number
---@field patch number
---@field prerelease? string
---@field build? string
---@field input? string

---@alias GitInfo {branch?: string, commit?: string, tag?:string, version?:Semver}

---@class LazyPkg
---@field name string
---@field dir string
---@field source "lazy" | "packspec" | "rockspec"
---@field file string
---@field spec LazyPluginSpec

---@alias LazyEvent {id:string, event:string[]|string, pattern?:string[]|string}
---@alias LazyEventSpec string|{event?:string|string[], pattern?:string|string[]}|string[]

---@class LazyKeysBase
---@field desc? string
---@field noremap? boolean
---@field remap? boolean
---@field expr? boolean
---@field nowait? boolean
---@field ft? string|string[]

---@class LazyKeysSpec: LazyKeysBase
---@field [1] string lhs
---@field [2]? string|fun()|false rhs
---@field mode? string|string[]

---@class LazyKeys: LazyKeysBase
---@field lhs string
---@field rhs? string
---@field mode? string
---@field id string
---@field name string

--- !! below, the default types from `lazy.types`

---@alias LazyPluginKind "normal"|"clean"|"disabled"

---@class LazyPluginState
---@field cache? table<string,any>
---@field cloned? boolean
---@field cond? boolean
---@field dep? boolean True if this plugin is only in the spec as a dependency
---@field dir? string Explicit dir or dev set for this plugin
---@field dirty? boolean
---@field build? boolean
---@field frags? number[]
---@field handlers? LazyPluginHandlers
---@field installed? boolean
---@field is_local? boolean
---@field kind? LazyPluginKind
---@field loaded? {[string]:string}|{time:number}
---@field outdated? boolean
---@field rtp_loaded? boolean
---@field tasks? LazyTask[]
---@field updated? {from:string, to:string}
---@field updates? {from:GitInfo, to:GitInfo}
---@field last_check? number
---@field working? boolean
---@field pkg? LazyPkg

---@alias PluginOpts table|fun(self:LazyPlugin, opts:table):table?

---@class LazyPluginHooks
---@field init? fun(self:LazyPlugin) Will always be run
---@field deactivate? fun(self:LazyPlugin) Unload/Stop a plugin
---@field config? fun(self:LazyPlugin, opts:table)|true Will be executed when loading the plugin
---@field build? false|string|async fun(self:LazyPlugin)|(string|async fun(self:LazyPlugin))[]
---@field opts? PluginOpts

---@class LazyPluginHandlers
---@field event? table<string,LazyEvent>
---@field ft? table<string,LazyEvent>
---@field keys? table<string,LazyKeys>
---@field cmd? table<string,string>

---@class LazyPluginRef
---@field branch? string
---@field tag? string
---@field commit? string
---@field version? string|boolean
---@field pin? boolean
---@field submodules? boolean Defaults to true

---@class LazyPluginBase
---@field [1] string?
---@field name string display name and name used for plugin config files
---@field main? string Entry module that has setup & deactivate
---@field url string?
---@field dir string
---@field enabled? boolean|(fun():boolean)
---@field cond? boolean|(fun():boolean)
---@field optional? boolean If set, then this plugin will not be added unless it is added somewhere else
---@field lazy? boolean
---@field priority? number Only useful for lazy=false plugins to force loading certain plugins first. Default priority is 50
---@field dev? boolean If set, then link to the respective folder under your ~/projects
---@field rocks? string[]

---@class LazyPlugin: LazyPluginBase,LazyPluginHandlers,LazyPluginHooks,LazyPluginRef
---@field dependencies? string[]
---@field specs? string|string[]|LazyPluginSpec[]
---@field _ LazyPluginState

---@class LazyPluginSpecHandlers
---@field event? string[]|string|LazyEventSpec[]|fun(self:LazyPlugin, event:string[]):string[]
---@field cmd? string[]|string|fun(self:LazyPlugin, cmd:string[]):string[]
---@field ft? string[]|string|fun(self:LazyPlugin, ft:string[]):string[]
---@field keys? string|string[]|LazyKeysSpec[]|fun(self:LazyPlugin, keys:string[]):((string|LazyKeys)[])
---@field module? false

---@class LazyPluginSpec: LazyPluginBase,LazyPluginSpecHandlers,LazyPluginHooks,LazyPluginRef
---@field dependencies? string|string[]|LazyPluginSpec[]
---@field specs? string|string[]|LazyPluginSpec[]

---@alias LazySpec string|LazyPluginSpec|LazySpecImport|LazySpec[]

---@class LazySpecImport
---@field import string|(fun():LazyPluginSpec) spec module to import
---@field name? string
---@field enabled? boolean|(fun():boolean)
---@field cond? boolean|(fun():boolean)

---@class LazyFragment
---@field id number
---@field pkg? boolean
---@field pid? number
---@field deps? number[]
---@field frags? number[]
---@field dep? boolean
---@field name string
---@field url? string
---@field dir? string
---@field spec LazyPlugin
