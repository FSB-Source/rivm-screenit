package nl.rivm.screenit.specification.algemeen;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.berichten.cda.MeldingOngeldigCdaBericht;
import nl.rivm.screenit.model.berichten.cda.MeldingOngeldigCdaBericht_;
import nl.rivm.screenit.model.berichten.cda.OntvangenCdaBericht_;
import nl.rivm.screenit.model.berichten.enums.BerichtType;
import nl.rivm.screenit.specification.ExtendedSpecification;

import org.springframework.data.jpa.domain.Specification;

import static nl.rivm.screenit.specification.SpecificationUtil.containsCaseInsensitive;
import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenEmpty;
import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenNull;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class MeldingOngeldigCdaBerichtSpecification
{
	public static Specification<MeldingOngeldigCdaBericht> heeftBerichtVanType(List<BerichtType> berichtTypes)
	{
		return (r, q, cb) -> join(r, MeldingOngeldigCdaBericht_.ontvangenCdaBericht).get(OntvangenCdaBericht_.berichtType).in(berichtTypes);
	}

	public static ExtendedSpecification<MeldingOngeldigCdaBericht> isActief()
	{
		return (r, q, cb) -> cb.isTrue(r.get(MeldingOngeldigCdaBericht_.actief));
	}

	public static Specification<MeldingOngeldigCdaBericht> filterOpBsn(String bsn)
	{
		return skipWhenEmpty(bsn, (r, q, cb) -> cb.equal(r.get(MeldingOngeldigCdaBericht_.bsn), bsn));
	}

	public static Specification<MeldingOngeldigCdaBericht> filterOpMeldingContaining(String melding)
	{
		return skipWhenEmpty(melding, (r, q, cb) -> containsCaseInsensitive(cb, r.get(MeldingOngeldigCdaBericht_.melding), melding));
	}

	public static Specification<MeldingOngeldigCdaBericht> filterOpScreeningOrganisatie(ScreeningOrganisatie screeningOrganisatie)
	{
		return skipWhenNull(screeningOrganisatie, (r, q, cb) -> cb.equal(r.get(MeldingOngeldigCdaBericht_.screeningOrganisatie), screeningOrganisatie));
	}
}
