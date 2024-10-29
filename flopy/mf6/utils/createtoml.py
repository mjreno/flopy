import json
import sys
from pathlib import Path

import toml

# parameter defaults
mf6_param_dfn = {
    "name": "",
    "type": "",
    "block_variable": False,
    "valid": [],
    "shape": "",
    "tagged": True,
    "in_record": False,
    "layered": False,
    "time_series": False,
    "jagged_array": "",
    "other_names": "",
    "reader": "",
    "optional": False,
    "preserve_case": False,
    "default_value": None,
    "numeric_index": False,
    "repeating": False,
    "support_negative_index": False,
    "just_data": False,
    "deprecated": "",
    "removed": "",
    "longname": "",
    "description": "",
}


class Dfn2Toml:
    """
    Verify MODFLOW 6 fortran source code format
    """

    def __init__(
        self,
        dfnfspec: str = None,
        outdir: str = None,
        strict: bool = True,
        complete: bool = False,
        verbose: bool = True,
    ):
        """dfn2toml init"""

        self._dfnfspec = Path(dfnfspec)
        self._outdir = (Path(outdir),)
        self._strict = strict
        self._complete = complete
        self._verbose = verbose
        self._var_d = {}
        self.component = ""
        self.subcomponent = ""
        self._multi_package = False
        self._stress_package = False
        self._advanced_package = False
        self._subpackage = {}

        self.component, self.subcomponent = self._dfnfspec.stem.upper().split(
            "-"
        )
        self._set_var_d()
        blocknames = self.get_blocknames()

        d = {
            "component": self.component,
            "subcomponent": self.subcomponent,
            "multi": False,
            "stress": False,
            "advanced": False,
            "subpackage": {},
            "blocks": {},
        }

        for b in blocknames:
            block_d = self._substitute(b, self.component, self.subcomponent)
            d["blocks"][b] = {}
            for p in block_d.keys():
                name = block_d[p]["name"]
                del block_d[p]["name"]
                d["blocks"][b][name] = block_d[p]
            # d["blocks"][b] = block_d

        if self._multi_package:
            d["multi"] = True
        if self._stress_package:
            d["stress"] = True
        if self._advanced_package:
            d["advanced"] = True
        if len(self._subpackage) == 6:
            d["subpackage"] = self._subpackage.copy()

        fname = f"{self.component.lower()}-{self.subcomponent.lower()}.toml"
        if self._verbose:
            print(f"  creating...{fname}")
        fspec = self._outdir[0] / fname
        with open(
            fspec,
            "w",
        ) as fh:
            toml.dump(d, fh)

    def _parse_comment(self, line):
        # parse comments for package scoped settings
        if "flopy multi-package" in line.strip():
            self._multi_package = True
        elif "package-type" in line.strip():
            pkg_tags = line.strip().split()
            if pkg_tags[2] == "stress-package":
                self._stress_package = True
            if pkg_tags[2] == "advanced-stress-package":
                self._stress_package = True
                self._advanced_package = True
        elif "flopy subpackage" in line.strip():
            tags = line.strip().split()[3:]
            if len(tags) == 4:
                self._subpackage["key"] = tags[0]
                self._subpackage["abbr"] = tags[1]
                self._subpackage["param"] = tags[2]
                self._subpackage["val"] = tags[3]
            else:
                pass
        elif "flopy parent_name_type" in line.strip():
            tags = line.strip().split()[2:]
            if len(tags) == 3:
                #self._subpackage["type"] = tags[0]
                self._subpackage["type"] = tags[1]
                self._subpackage["objects"] = tags[2].split("/")
            else:
                pass

    def _param_key(self, param_dfn, component_d):
        name = param_dfn["name"]
        if "block" in param_dfn:
            block = param_dfn["block"]
            key = (name, block)
        else:
            key = name
        if name in component_d:
            raise Exception(
                f"{self.component}-{self.subcomponent} variable already exists in dictionary: {name}"
            )
        return key

    def _set_var_d(self):
        f = open(self._dfnfspec, "r")
        lines = f.readlines()
        f.close()

        # component param dictionary
        params_d = {}

        # currently processed param dfn
        param_dfn = {}

        for line in lines:
            # blank lines separate param definitions
            if len(line.strip()) == 0:
                if len(param_dfn) > 0:
                    param_key = self._param_key(param_dfn, params_d)
                    params_d[param_key] = param_dfn
                param_dfn = {}
                continue

            # parse comments for package scoped settings
            if "#" in line.strip()[0]:
                self._parse_comment(line)
                continue

            # add attribute to param dfn
            tokens = line.strip().split()
            if len(tokens) > 1:
                attr = tokens[0]
                istart = line.index(" ")
                v = line[istart:].strip()
                if attr in param_dfn:
                    raise Exception(
                        f"{self.component}-{self.subcomponent} attribute already exists in dictionary: {attr}"
                    )
                param_dfn[attr] = v

        # capture last param dfn
        if len(param_dfn) > 0:
            param_key = self._param_key(param_dfn, params_d)
            params_d[param_key] = param_dfn

        # assign param dictionary
        self._var_d = params_d

        # 
        if self._strict:
            self._verify_dfn_attributes()

    def _verify_dfn_attributes(self):
        for p in self._var_d:
            v = self._var_d[p]
            for k in v.keys():
                if k.lower() not in mf6_param_dfn.keys():
                    if (
                        k.lower() != "block"
                        and k.lower() != "name"
                        and k.lower() != "mf6internal"
                    ):
                        raise Exception(
                            f"{self.component}-{self.subcomponent} {varname} unhandled attribute: {k.lower()}"
                        )

    def _nest_composite(self, blockname, composite_d, block_d):
        block_params = list(block_d)
        composite_params = []

        for param in list(composite_d):
            params = composite_d[param]["type"].split()[1:]
            composite_params += params
            composite_d[param]["parameters"] = {}
            for p in params:
                if p in list(block_d):
                    composite_d[param]["parameters"][p] = block_d[p]
                elif p in composite_d:
                    composite_d[param]["parameters"][p] = composite_d[p]
                else:
                    raise Exception(
                        f"{self.component}-{self.subcomponent} {blockname} composite param definition not found: "
                        + p
                    )
                if "name" in list(composite_d[param]["parameters"][p]):
                    del composite_d[param]["parameters"][p]["name"]

        for p in composite_params:
            if p in list(block_d):
                del block_d[p]
            elif p in composite_d:
                del composite_d[p]

    def _substitute(self, blockname, component, subcomponent):
        block_d = {}
        composite_d = {}

        for k in self._var_d:
            varname, block = k
            if block != blockname:
                continue

            v = self._var_d[k]

            vtype = v["type"].lower()
            #if vtype == "double precision":
            #    vtype = "double"

            d = None
            if self._complete:
                d = mf6_param_dfn.copy()
            else:
                d = v.copy()
                del d["block"]
            for k in mf6_param_dfn.keys():
                if k in v:
                    if isinstance(mf6_param_dfn[k], bool):
                        if v[k].lower() == "true":
                            d[k] = True
                        elif v[k].lower() == "false":
                            d[k] = False
                    elif k == "valid":
                        valid = v[k].strip().split()
                        if len(valid) > 0:
                            d[k] = valid.copy()
                    elif k == "type":
                        d[k] = vtype
                    elif k == "description":
                        d[k] = v[k].replace("\\", "").strip()
                    else:
                        d[k] = v[k]

            if (
                d["type"].lower().startswith("recarray")
                or d["type"].lower().startswith("record")
                or d["type"].lower().startswith("recordrepeating")
                or d["type"].lower().startswith("keystring")
            ):
                composite_d[varname] = d
            else:
                block_d[varname] = d

        if len(composite_d) > 0:
            self._nest_composite(blockname, composite_d, block_d)

        return {**block_d, **composite_d}

    def get_blocknames(self):
        blocknames = []
        for var, block in self._var_d:
            if block not in blocknames:
                blocknames.append(block.strip())
        return blocknames
