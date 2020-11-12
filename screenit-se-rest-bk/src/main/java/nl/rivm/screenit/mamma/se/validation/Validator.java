package nl.rivm.screenit.mamma.se.validation;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
 * %%
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * =========================LICENSE_END==================================
 */

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.mamma.se.dto.actions.ActionDto;
import nl.rivm.screenit.mamma.se.dto.actions.SEActieType;

public abstract class Validator {

    protected abstract List<SEActieType> actieTypes();

    protected abstract void validateActions(List<ActionDto> actions);

    public final void validate(List<ActionDto> actions) throws IOException {
        validateActions(actionsOfAppropriateTypes(actions));
    }

    private List<ActionDto> actionsOfAppropriateTypes(List<ActionDto> actions) {
        Map<SEActieType, ActionDto> map = new HashMap<>();
        for (ActionDto action : actions) {
            map.put(action.getType(), action); 
        }
        List<ActionDto> result = new ArrayList<>();
        for (SEActieType actieType : actieTypes()) {
            if (map.get(actieType) != null) {
                result.add(map.get(actieType));
            }
        }
        return result;
    }
}
