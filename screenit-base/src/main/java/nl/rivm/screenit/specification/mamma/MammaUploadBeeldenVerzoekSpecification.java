package nl.rivm.screenit.specification.mamma;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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

import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.mamma.MammaUploadBeeldenVerzoek;
import nl.rivm.screenit.model.mamma.MammaUploadBeeldenVerzoekStatus;
import nl.rivm.screenit.model.mamma.MammaUploadBeeldenVerzoek_;
import nl.rivm.screenit.specification.ExtendedSpecification;
import nl.rivm.screenit.specification.SpecificationUtil;

import org.springframework.data.jpa.domain.Specification;

import static nl.rivm.screenit.specification.algemeen.OrganisatieMedewerkerSpecification.heeftInstelling;

@NoArgsConstructor(access = lombok.AccessLevel.PRIVATE)
public class MammaUploadBeeldenVerzoekSpecification
{
	public static ExtendedSpecification<MammaUploadBeeldenVerzoek> heeftSatusIn(List<MammaUploadBeeldenVerzoekStatus> statussen)
	{
		return (r, q, cb) -> r.get(MammaUploadBeeldenVerzoek_.status).in(statussen);
	}

	public static ExtendedSpecification<MammaUploadBeeldenVerzoek> heeftZiekenhuis(Instelling ziekenhuis)
	{
		return (r, q, cb) -> cb.equal(r.get(MammaUploadBeeldenVerzoek_.ziekenhuis), ziekenhuis);
	}

	public static ExtendedSpecification<MammaUploadBeeldenVerzoek> filterOpZiekenhuis(Instelling ziekenhuis)
	{
		return SpecificationUtil.skipWhenNullExtended(ziekenhuis, heeftZiekenhuis(ziekenhuis));
	}

	public static Specification<MammaUploadBeeldenVerzoek> filterOpGemaaktDoorOrganisatie(Instelling organisatie)
	{
		return SpecificationUtil.skipWhenNull(organisatie, heeftInstelling(organisatie).with(MammaUploadBeeldenVerzoek_.gemaaktDoor));
	}

}
