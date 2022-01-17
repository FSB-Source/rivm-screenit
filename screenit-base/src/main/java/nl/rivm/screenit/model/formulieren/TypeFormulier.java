
package nl.rivm.screenit.model.formulieren;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.model.cervix.verslag.cytologie.CervixCytologieVerslagContent;
import nl.rivm.screenit.model.colon.verslag.mdl.MdlVerslagContent;
import nl.rivm.screenit.model.colon.verslag.pa.PaVerslagContent;
import nl.rivm.screenit.model.mamma.verslag.followup.MammaFollowUpVerslagContent;

public enum TypeFormulier
{
	MDL(MdlVerslagContent.class),

	PALGA(PaVerslagContent.class),

	CYTOLOGIE(CervixCytologieVerslagContent.class),

	MAMMA_PA_FOLLOW_UP(MammaFollowUpVerslagContent.class),

	VRAGENLIJST(Object.class)

	;

	private Class<?> rootEntityClass;

	private TypeFormulier(Class<?> rootEntityClass)
	{
		this.rootEntityClass = rootEntityClass;
	}

	public Class<?> getRootEntityClass()
	{
		return rootEntityClass;
	}
}
