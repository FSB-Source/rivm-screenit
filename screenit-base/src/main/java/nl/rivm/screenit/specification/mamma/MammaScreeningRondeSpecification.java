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
import java.util.Date;
import java.util.List;

import javax.persistence.criteria.Join;
import javax.persistence.criteria.Root;

import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.SingleTableHibernateObject_;
import nl.rivm.screenit.model.mamma.MammaAfspraak_;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaBeoordeling_;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaDossier_;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.MammaOnderzoek_;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde_;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode_;
import nl.rivm.screenit.model.mamma.MammaStandplaatsRonde_;
import nl.rivm.screenit.model.mamma.MammaUitnodiging_;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.model.mamma.enums.MammaMammografieIlmStatus;
import nl.rivm.screenit.specification.ExtendedSpecification;

import org.springframework.data.jpa.domain.Specification;

import static nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus.UITSLAG_GUNSTIG;
import static nl.rivm.screenit.model.mamma.enums.MammaMammografieIlmStatus.BESCHIKBAAR;
import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenNull;
import static nl.rivm.screenit.specification.algemeen.ScreeningRondeSpecification.isAfgerond;

@NoArgsConstructor(access = lombok.AccessLevel.PRIVATE)
public class MammaScreeningRondeSpecification
{
	public static Specification<MammaScreeningRonde> heeftClient(Client client)
	{
		return (r, q, cb) ->
		{
			var dossierJoin = join(r, MammaScreeningRonde_.dossier);
			var clientJoin = join(dossierJoin, MammaDossier_.client); 
			return cb.equal(clientJoin.get(SingleTableHibernateObject_.id), client.getId());
		};
	}

	public static Specification<MammaScreeningRonde> heeftBeoordelingStatusIn(List<MammaBeoordelingStatus> beoordelingStatuses)
	{
		return (r, q, cb) -> beoordelingenJoin(r).get(MammaBeoordeling_.status).in(beoordelingStatuses);
	}

	public static Specification<MammaScreeningRonde> heeftLaatsteOnderzoekBeoordelingStatus(MammaBeoordelingStatus beoordelingStatus)
	{
		return (r, q, cb) -> cb.equal(laatsteBeoordelingenJoin(r).get(MammaBeoordeling_.status), beoordelingStatus);
	}

	public static Specification<MammaScreeningRonde> filterOpBeoordelingVoorDatum(LocalDate voorDatum)
	{
		return skipWhenNull(voorDatum, MammaBeoordelingSpecification.heeftStatusDatumVoor(voorDatum).withRoot(MammaScreeningRondeSpecification::beoordelingenJoin));
	}

	public static Specification<MammaScreeningRonde> heeftStandplaatsPeriodeDieAflooptOpOfNa(Date datum)
	{
		return (r, q, cb) ->
		{
			var standplaatsRondeJoin = join(r, MammaScreeningRonde_.standplaatsRonde);
			var standplaatsPeriodeJoin = join(standplaatsRondeJoin, MammaStandplaatsRonde_.standplaatsPerioden);
			return cb.and(cb.greaterThanOrEqualTo(standplaatsPeriodeJoin.get(MammaStandplaatsPeriode_.totEnMet), datum));
		};
	}

	public static Specification<MammaScreeningRonde> heeftStandplaats(MammaStandplaats standplaats)
	{
		return (r, q, cb) ->
		{
			var standplaatsRondeJoin = join(r, MammaScreeningRonde_.standplaatsRonde);
			return cb.equal(standplaatsRondeJoin.get(MammaStandplaatsRonde_.standplaats), standplaats);
		};
	}

	private static Join<MammaOnderzoek, MammaBeoordeling> beoordelingenJoin(Root<MammaScreeningRonde> r)
	{
		var uitnodigingen = join(r, MammaScreeningRonde_.uitnodigingen);
		var afspraken = join(uitnodigingen, MammaUitnodiging_.afspraken);
		var onderzoeken = join(afspraken, MammaAfspraak_.onderzoek);
		return join(onderzoeken, MammaOnderzoek_.beoordelingen);
	}

	private static Join<MammaOnderzoek, MammaBeoordeling> laatsteBeoordelingenJoin(Root<MammaScreeningRonde> r)
	{
		var laatsteOnderzoek = join(r, MammaScreeningRonde_.laatsteOnderzoek);
		return join(laatsteOnderzoek, MammaOnderzoek_.laatsteBeoordeling);
	}

	public static Specification<MammaScreeningRonde> heeftBeeldenMetGunstigeUitslag(Root<MammaDossier> dossierRoot)
	{
		return heeftBeeldenMetGunstigeUitslag().and(heeftDossier(dossierRoot));
	}

	public static ExtendedSpecification<MammaScreeningRonde> heeftGeenUitstel()
	{
		return (r, q, cb) -> cb.isNull(r.get(MammaScreeningRonde_.laatsteUitstel));
	}

	public static Specification<MammaScreeningRonde> heeftBeeldenMetGunstigeUitslag(MammaDossier dossier)
	{
		return heeftBeeldenMetGunstigeUitslag().and(heeftDossier(dossier));
	}

	private static Specification<MammaScreeningRonde> heeftBeeldenMetGunstigeUitslag()
	{
		return heeftLaatsteOnderzoekBeoordelingStatus(UITSLAG_GUNSTIG)
			.and(heeftMammografieIlmStatus(BESCHIKBAAR))
			.and(isAfgerond());
	}

	public static ExtendedSpecification<MammaScreeningRonde> heeftDossier(MammaDossier dossier)
	{
		return (r, q, cb) -> cb.equal(r.get(MammaScreeningRonde_.dossier), dossier);
	}

	public static Specification<MammaScreeningRonde> heeftDossier(Root<MammaDossier> mammaDossierRoot)
	{
		return (r, q, cb) -> cb.equal(r.get(MammaScreeningRonde_.dossier), mammaDossierRoot);
	}

	public static ExtendedSpecification<MammaDossier> heeftPreciesEenRonde()
	{
		return (r, q, cb) ->
		{
			var subQuery = q.subquery(Long.class);
			var subQueryRoot = subQuery.from(MammaScreeningRonde.class);
			subQuery.select(cb.count(cb.literal(1))) 
				.where(cb.equal(subQueryRoot.get(MammaScreeningRonde_.dossier), r));
			return cb.equal(subQuery, 1);
		};
	}

	public static ExtendedSpecification<MammaScreeningRonde> heeftGeenFollowUpConclusie()
	{
		return (r, q, cb) -> cb.isNull(r.get(MammaScreeningRonde_.followUpConclusieStatus));
	}

	public static Specification<MammaScreeningRonde> heeftMammografieIlmStatus(MammaMammografieIlmStatus ilmStatus)
	{
		return MammaMammografieBaseSpecification.heeftIlmStatus(ilmStatus).with(r ->
		{
			var onderzoekJoin = join(r, MammaScreeningRonde_.laatsteOnderzoek);
			return join(onderzoekJoin, MammaOnderzoek_.mammografie);
		});
	}

	public static ExtendedSpecification<MammaScreeningRonde> isMinderValideOnderzoekZiekenhuis(boolean minderValideOnderzoekZiekenhuis)
	{
		return (r, q, cb) -> cb.equal(r.get(MammaScreeningRonde_.minderValideOnderzoekZiekenhuis), minderValideOnderzoekZiekenhuis);
	}

	public static ExtendedSpecification<MammaScreeningRonde> heeftGeenScreeningRondeEvent()
	{
		return (r, q, cb) -> cb.isNull(r.get(MammaScreeningRonde_.screeningRondeEvent));
	}
}
