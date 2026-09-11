local publish = [[set -euo pipefail; python3 .forge/publish]]

return {
    select = function(ctx)
        if string.match(ctx.ref_name, "^refs/tags/v") then
            return { "publish", "publish-npm" }
        end
        return {}
    end,
    publish = function(_)
        return {
            output = "package",
            base = publish,
            packages = {
                publish = {
                    { registry = "41021897", ecosystem = "nuget", mode = "explicit", prefix = "Rbt.Pdf" },
                },
            },
        }
    end,
    ["publish-npm"] = function(_)
        return {
            output = "package",
            base = ".forge/publish-npm",
            packages = {
                publish = {
                    { registry = "41021897", ecosystem = "npm", mode = "explicit", prefix = "@rbt/pdf" },
                },
            },
        }
    end,
}
