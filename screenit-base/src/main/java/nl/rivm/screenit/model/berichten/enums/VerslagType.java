
package nl.rivm.screenit.model.berichten.enums;

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

import nl.rivm.screenit.model.berichten.Verslag;
import nl.rivm.screenit.model.berichten.cda.CdaOID;
import nl.rivm.screenit.model.cervix.CervixCytologieVerslag;
import nl.rivm.screenit.model.colon.MdlVerslag;
import nl.rivm.screenit.model.colon.PaVerslag;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.formulieren.TypeFormulier;
import nl.rivm.screenit.model.mamma.MammaFollowUpVerslag;

public enum VerslagType
{
	MDL(
		Bevolkingsonderzoek.COLON,
		TypeFormulier.MDL,
		VerslagGeneratie.V8,
		CdaOID.CONCEPTS_ROOT_OID,
		LogGebeurtenis.MDL_VERSLAG_VERWIJDERD,
		LogGebeurtenis.MDL_VERSLAG_HEROPEND,
		MdlVerslag.class),

	PA_LAB(
		Bevolkingsonderzoek.COLON,
		TypeFormulier.PALGA,
		VerslagGeneratie.V8,
		CdaOID.CONCEPTS_ROOT_OID,
		LogGebeurtenis.PA_VERSLAG_VERWIJDERD,
		LogGebeurtenis.PA_VERSLAG_HEROPEND,
		PaVerslag.class),

	CERVIX_CYTOLOGIE(
		Bevolkingsonderzoek.CERVIX,
		TypeFormulier.CYTOLOGIE,
		VerslagGeneratie.V8,
		CdaOID.CONCEPTS_ROOT_OID,
		null,
		null,
		CervixCytologieVerslag.class),

	MAMMA_PA_FOLLOW_UP(
		Bevolkingsonderzoek.MAMMA,
		TypeFormulier.MAMMA_PA_FOLLOW_UP,
		VerslagGeneratie.V2,
		CdaOID.CONCEPTS_ROOT_OID_BK,
		LogGebeurtenis.MAMMA_VERSLAG_FOLLOW_UP_VERWIJDERD,
		LogGebeurtenis.MAMMA_VERSLAG_FOLLOW_UP_HEROPEND,
		MammaFollowUpVerslag.class);

	private final Bevolkingsonderzoek bevolkingsonderzoek;

	private final TypeFormulier typeFormulier;

	private final VerslagGeneratie huidigeGeneratie;

	private final String conceptRootOid;

	private final Class<? extends Verslag<?, ?>> clazz;

	private final LogGebeurtenis verwijderdVerslagLogGebeurtenis;

	private final LogGebeurtenis heropendVerslagLogGebeurtenis;

	private VerslagType(Bevolkingsonderzoek bevolkingsonderzoek, TypeFormulier typeFormulier, VerslagGeneratie huidigeGeneratie, String conceptRootOid,
		LogGebeurtenis verwijderdVerslagLogGebeurtenis, LogGebeurtenis heropendVerslagLogGebeurtenis, Class<? extends Verslag<?, ?>> clazz)
	{
		this.bevolkingsonderzoek = bevolkingsonderzoek;
		this.typeFormulier = typeFormulier;
		this.huidigeGeneratie = huidigeGeneratie;
		this.conceptRootOid = conceptRootOid;
		this.verwijderdVerslagLogGebeurtenis = verwijderdVerslagLogGebeurtenis;
		this.heropendVerslagLogGebeurtenis = heropendVerslagLogGebeurtenis;
		this.clazz = clazz;
	}

	public Bevolkingsonderzoek getBevolkingsonderzoek()
	{
		return bevolkingsonderzoek;
	}

	public TypeFormulier getTypeFormulier()
	{
		return typeFormulier;
	}

	public VerslagGeneratie getHuidigeGeneratie()
	{
		return huidigeGeneratie;
	}

	public String getConceptRootOid()
	{
		return conceptRootOid;
	}

	public Class<? extends Verslag<?, ?>> getClazz()
	{
		return clazz;
	}

	public LogGebeurtenis getVerwijderdVerslagLogGebeurtenis()
	{
		return verwijderdVerslagLogGebeurtenis;
	}

	public LogGebeurtenis getHeropendVerslagLogGebeurtenis()
	{
		return heropendVerslagLogGebeurtenis;
	}
}
