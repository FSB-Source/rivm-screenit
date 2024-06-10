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

import java.time.LocalDateTime;
import java.util.List;

import javax.persistence.criteria.From;
import javax.persistence.criteria.Root;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;

import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaAfspraak_;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde_;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode_;
import nl.rivm.screenit.model.mamma.MammaUitnodiging_;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.model.mamma.enums.MammaDoelgroep;
import nl.rivm.screenit.specification.SpecificationUtil;

import org.springframework.data.jpa.domain.Specification;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class MammaAfspraakSpecification
{
	public static Specification<MammaAfspraak> heeftStatuses(List<MammaAfspraakStatus> statuses)
	{
		return (r, q, cb) -> r.get(MammaAfspraak_.status).in(statuses);
	}

	public static Specification<MammaAfspraak> begintTussen(LocalDateTime vanaf, LocalDateTime tot)
	{
		return SpecificationUtil.betweenLocalDateTimes(vanaf, tot, r -> r.get(MammaAfspraak_.vanaf));
	}

	public static Specification<MammaAfspraak> heeftGeenCapaciteitBlok()
	{
		return (r, q, cb) -> cb.isNull(r.get(MammaAfspraak_.capaciteitBlok));
	}

	public static Specification<MammaAfspraak> heeftClientInTehuis()
	{
		return MammaDossierSpecification.woontInTehuisPredicate().toSpecification(MammaAfspraakSpecification::dossierJoin);
	}

	public static Specification<MammaAfspraak> heeftGeenClientInTehuis()
	{
		return MammaDossierSpecification.woontNietInTehuisPredicate().toSpecification(MammaAfspraakSpecification::dossierJoin);
	}

	public static Specification<MammaAfspraak> heeftDoelgroep(List<MammaDoelgroep> doelgroepen)
	{
		return MammaDossierSpecification.heeftDoelgroep(doelgroepen).toSpecification(MammaAfspraakSpecification::dossierJoin);
	}

	public static Specification<MammaAfspraak> heeftScreeningsEenheid(MammaScreeningsEenheid screeningsEenheid)
	{
		return (r, q, cb) ->
		{
			var standplaatsPeriodeJoin = SpecificationUtil.join(r, MammaAfspraak_.standplaatsPeriode);
			return cb.equal(standplaatsPeriodeJoin.get(MammaStandplaatsPeriode_.screeningsEenheid), screeningsEenheid);
		};
	}

	private static From<MammaScreeningRonde, MammaDossier> dossierJoin(Root<MammaAfspraak> afspraakRoot)
	{
		var uitnodigingJoin = SpecificationUtil.join(afspraakRoot, MammaAfspraak_.uitnodiging);
		var screeningRondeJoin = SpecificationUtil.join(uitnodigingJoin, MammaUitnodiging_.screeningRonde);
		return SpecificationUtil.join(screeningRondeJoin, MammaScreeningRonde_.dossier);
	}
}
