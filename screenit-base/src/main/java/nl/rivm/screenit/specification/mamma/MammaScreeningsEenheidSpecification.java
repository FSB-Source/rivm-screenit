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

import java.time.LocalDate;
import java.util.List;

import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.BeoordelingsEenheid;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid_;
import nl.rivm.screenit.specification.ExtendedSpecification;
import nl.rivm.screenit.specification.algemeen.BeoordelingsEenheidSpecification;
import nl.rivm.screenit.util.DateUtil;

import org.springframework.data.jpa.domain.Specification;

import static nl.rivm.screenit.specification.SpecificationUtil.containsCaseInsensitive;
import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenEmpty;
import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenNull;

@NoArgsConstructor(access = lombok.AccessLevel.PRIVATE)
public class MammaScreeningsEenheidSpecification
{
	public static Specification<MammaScreeningsEenheid> heeftCode(String code)
	{
		return (r, q, cb) -> cb.equal(r.get(MammaScreeningsEenheid_.code), code);
	}

	public static Specification<MammaScreeningsEenheid> filterCodeContaining(String code)
	{
		return skipWhenEmpty(code, (r, q, cb) -> containsCaseInsensitive(cb, r.get(MammaScreeningsEenheid_.code), code));
	}

	public static Specification<MammaScreeningsEenheid> filterNaamContaining(String naam)
	{
		return skipWhenEmpty(naam, (r, q, cb) -> containsCaseInsensitive(cb, r.get(MammaScreeningsEenheid_.naam), naam));
	}

	public static Specification<MammaScreeningsEenheid> isActief()
	{
		return (r, q, cb) -> cb.isTrue(r.get(MammaScreeningsEenheid_.actief));
	}

	public static Specification<MammaScreeningsEenheid> filterActief(Boolean actief)
	{
		return skipWhenNull(actief, (r, q, cb) -> cb.equal(r.get(MammaScreeningsEenheid_.actief), actief));
	}

	public static Specification<MammaScreeningsEenheid> heeftIpAdres(String ipAdres)
	{
		return (r, q, cb) -> cb.equal(r.get(MammaScreeningsEenheid_.ipAdres), ipAdres);
	}

	public static Specification<MammaScreeningsEenheid> heeftBeoordelingsEenheid(BeoordelingsEenheid beoordelingsEenheid)
	{
		return (r, q, cb) -> cb.equal(r.get(MammaScreeningsEenheid_.beoordelingsEenheid), beoordelingsEenheid);
	}

	public static Specification<MammaScreeningsEenheid> filterBeoordelingsEenheid(BeoordelingsEenheid beoordelingsEenheid)
	{
		return skipWhenNull(beoordelingsEenheid, heeftBeoordelingsEenheid(beoordelingsEenheid));
	}

	public static Specification<MammaScreeningsEenheid> heeftBeoordelingsEenheidIn(List<BeoordelingsEenheid> beoordelingsEenheden)
	{
		return (r, q, cb) -> r.get(MammaScreeningsEenheid_.beoordelingsEenheid).in(beoordelingsEenheden);
	}

	public static Specification<MammaScreeningsEenheid> heeftTijdelijkeBeoordelingsEenheidActiefOpMoment(BeoordelingsEenheid beoordelingsEenheid, LocalDate moment)
	{
		return (r, q, cb) -> cb.and(cb.equal(r.get(MammaScreeningsEenheid_.tijdelijkeBeoordelingsEenheid), beoordelingsEenheid),
			cb.greaterThanOrEqualTo(r.get(MammaScreeningsEenheid_.tijdelijkeBeTotEnMetDatum), DateUtil.toUtilDate(moment)));
	}

	public static ExtendedSpecification<MammaScreeningsEenheid> heeftScreeningsOrganisatie(ScreeningOrganisatie screeningOrganisatie)
	{
		return BeoordelingsEenheidSpecification.heeftScreeningOrganisatie(screeningOrganisatie).with(MammaScreeningsEenheid_.beoordelingsEenheid);
	}

	public static Specification<MammaScreeningsEenheid> filterScreeningsOrganisatie(ScreeningOrganisatie screeningOrganisatie)
	{
		return skipWhenNull(screeningOrganisatie, heeftScreeningsOrganisatie(screeningOrganisatie));
	}

	public static ExtendedSpecification<MammaScreeningsEenheid> heeftVrijgegevenTotEnMetOpOfNaDatum(LocalDate datum)
	{
		return (r, q, cb) -> cb.greaterThanOrEqualTo(r.get(MammaScreeningsEenheid_.vrijgegevenTotEnMet), DateUtil.toUtilDate(datum));
	}
}
