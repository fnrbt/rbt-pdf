-- Forge allows one publish entry per registry and ecosystem in an action, so
-- each package is published by its own action.
local function package(id)
    return function(_)
        return {
            output = "package",
            base = "set -euo pipefail; python3 .forge/publish " .. id,
            packages = {
                -- Read access makes the sandbox write a NuGet.Config that goes
                -- through its credentialed gateway. Without it the restore
                -- queries the registry anonymously and fails with 401.
                read = {
                    { registry = "41021897", ecosystem = "nuget" },
                },
                publish = {
                    { registry = "41021897", ecosystem = "nuget", mode = "explicit", prefix = id },
                },
            },
        }
    end
end

return {
    select = function(ctx)
        -- This isolated writer release must not publish renderer/npm packages.
        if ctx.ref_name == "refs/tags/v0.1.20" then
            return { "publish" }
        end
        if string.match(ctx.ref_name, "^refs/tags/v%d+%.%d+%.%d+") then
            return { "publish", "publish-render", "publish-npm" }
        end
        return {}
    end,
    ["publish"] = package("Rbt.Pdf"),
    -- Rbt.Pdf.Render's reference to Rbt.Pdf is built from source here and
    -- becomes a package dependency at the same version; its restore needs
    -- only Rbt.Renderer, which is already on the feed.
    ["publish-render"] = package("Rbt.Pdf.Render"),
    ["publish-npm"] = function(_)
        return {
            output = "package",
            base = ".forge/publish-npm",
            packages = {
                -- Restores the Rbt libraries the viewer is compiled from.
                read = {
                    { registry = "41021897", ecosystem = "nuget" },
                },
                publish = {
                    { registry = "41021897", ecosystem = "npm", mode = "explicit", prefix = "@rbt/pdf" },
                },
            },
        }
    end,
}
