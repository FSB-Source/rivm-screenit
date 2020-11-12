package nl.rivm.screenit.service.mamma;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.util.List;
import java.util.Map;

import nl.rivm.screenit.model.mamma.MammaArchitectuurverstoringLaesie;
import nl.rivm.screenit.model.mamma.MammaAsymmetrieLaesie;
import nl.rivm.screenit.model.mamma.MammaCalcificatiesLaesie;
import nl.rivm.screenit.model.mamma.MammaLaesie;
import nl.rivm.screenit.model.mamma.MammaLezing;
import nl.rivm.screenit.model.mamma.MammaMassaLaesie;
import nl.rivm.screenit.service.mamma.be.verslag.MammaLaesieTypeMergeField;

public interface MammaBaseLaesieService
{

	Map<MammaLaesieTypeMergeField, String> getBaseLaesieMap(MammaLaesie laesie);

	Map<MammaLaesieTypeMergeField, String> getAsymetrieLaesieMap(MammaAsymmetrieLaesie laesie);

	Map<MammaLaesieTypeMergeField, String> getArchitectuurVerstoringMap(MammaArchitectuurverstoringLaesie laesie);

	Map<MammaLaesieTypeMergeField, String> getCalificatiesMap(MammaCalcificatiesLaesie laesie);

	Map<MammaLaesieTypeMergeField, String> getMassaMap(MammaMassaLaesie laesie);

	String getAllLaesieTekstVoorVerslagLezing(MammaLezing verslaglezing);

	boolean isVolgnummerNodig(List<MammaLaesie> alleLaesies, MammaLaesie laesie);
}
