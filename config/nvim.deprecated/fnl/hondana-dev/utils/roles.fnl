(import-macros {: tc} :hondana-dev.macros)

(tc available_roles "Role[]")
(local available-roles [:developer
                        :rustacean
                        :haskell-cultist
                        :shenturion
                        :commentator])

(local Roles {})
(set Roles.__index Roles)

(tc param _3fdata? Role param _3fcustom_roles? "string[]" return Roles)
(fn Roles.new [self ?data ?custom-roles]
  "Make a role record. `custom-roles` disables the type checking"
  (tc roles "Role[]")
  (when ?custom-roles
    (for [i 1 (length ?custom-roles)]
      (table.insert available-roles (. ?custom-roles i))))
  ;;
  (tc roles "Role[]")
  (local roles [])
  (tc param role Role return boolean)

  (fn is-permitted [role]
    (vim.list_contains available-roles role))

  (fn self.set [self data]
    (for [i 1 (length data)]
      (self:add (. data i))))

  (fn self.add [self role]
    (when (not (self:check role)) (table.insert roles role)))

  (fn self.remove [_ role]
    (if (is-permitted role)
        (when (not (table.remove roles role)) (error (.. role " was not set")))
        (error (.. role " is not a role"))))

  (fn self.check [_ role]
    (if (is-permitted role)
        (vim.list_contains roles role)
        (error (.. role " is not a role"))))

  (when ?data (self:set ?data))
  (setmetatable {} self))

(tc class Roles ;;
    field set "fun(self: Roles, data: Role[]): nil" ;;
    field add "fun(self: Roles, role: Role): nil" ;;
    field remove "fun(self: Roles, role: Role): nil" ;;
    field check "fun(self: Roles, role: Role): boolean")

Roles
