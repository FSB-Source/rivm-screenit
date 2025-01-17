package nl.rivm.screenit.repository.mamma;

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

import java.util.Collection;
import java.util.Date;
import java.util.List;

import javax.persistence.criteria.Join;
import javax.persistence.criteria.JoinType;
import javax.persistence.criteria.Root;

import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaAfspraak_;
import nl.rivm.screenit.model.mamma.MammaCapaciteitBlok;
import nl.rivm.screenit.model.mamma.MammaCapaciteitBlok_;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaDossier_;
import nl.rivm.screenit.model.mamma.MammaOpkomstkans;
import nl.rivm.screenit.model.mamma.MammaOpkomstkans_;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde_;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.model.mamma.MammaUitnodiging_;
import nl.rivm.screenit.model.mamma.enums.MammaCapaciteitBlokType;
import nl.rivm.screenit.repository.BaseJpaRepository;
import nl.rivm.screenit.specification.mamma.MammaCapaciteitBlokSpecification;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject_;

import org.springframework.data.domain.Sort;

import com.google.common.collect.Range;

import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.mamma.MammaCapaciteitBlokSpecification.voorScreeningsEenheidInPeriode;

public interface MammaCapaciteitBlokRepository extends BaseJpaRepository<MammaCapaciteitBlok>
{

	default List<MammaCapaciteitBlokProjectie> findNietGeblokkeerdeCapaciteitBlokken(Range<Date> zoekPeriode, Collection<MammaCapaciteitBlokType> blokTypes,
		MammaScreeningsEenheid screeningsEenheid, ScreeningOrganisatie screeningOrganisatie, MammaStandplaats standplaats)
	{
		return findWith(
			voorScreeningsEenheidInPeriode(screeningsEenheid, blokTypes, zoekPeriode)
				.and(MammaCapaciteitBlokSpecification.metActieveAfspraken())
				.and(MammaCapaciteitBlokSpecification.heeftGeenActieveBlokkade(zoekPeriode, screeningsEenheid, screeningOrganisatie, standplaats)),
			MammaCapaciteitBlokProjectie.class,
			q -> q.sortBy(Sort.by(AbstractHibernateObject_.ID))
				.sortBy(Sort.by(MammaCapaciteitBlok_.AFSPRAKEN + "." + MammaAfspraak_.VANAF))
				.projections((cb, r) ->
				{
					var afspraakJoin = afspraakJoin(r);
					var dossierJoin = dossierJoin(afspraakJoin);
					return List.of(
						r.get(AbstractHibernateObject_.id).alias("blokId"),
						r.get(MammaCapaciteitBlok_.vanaf).alias("blokVanaf"),
						r.get(MammaCapaciteitBlok_.tot).alias("blokTot"),
						r.get(MammaCapaciteitBlok_.blokType).alias("blokType"),
						r.get(MammaCapaciteitBlok_.aantalOnderzoeken).alias("aantalOnderzoeken"),
						r.get(MammaCapaciteitBlok_.minderValideAfspraakMogelijk).alias("minderValideAfspraakMogelijk"),
						afspraakJoin.get(MammaAfspraak_.vanaf).alias("afspraakVanaf"),
						opkomstkansJoin(afspraakJoin).get(MammaOpkomstkans_.opkomstkans).alias("opkomstkans"),
						dossierJoin.get(MammaDossier_.doelgroep).alias("doelgroep"),
						dossierJoin.get(MammaDossier_.tehuis).get(AbstractHibernateObject_.id).alias("tehuisId"),
						dossierJoin.get(MammaDossier_.eersteOnderzoek).alias("eersteOnderzoek")
					);
				})
				.all());
	}

	private Join<MammaCapaciteitBlok, MammaAfspraak> afspraakJoin(Root<MammaCapaciteitBlok> r)
	{
		return join(r, MammaCapaciteitBlok_.afspraken, JoinType.LEFT);
	}

	private Join<MammaScreeningRonde, MammaDossier> dossierJoin(Join<MammaCapaciteitBlok, MammaAfspraak> afspraakJoin)
	{
		var uitnodigingJoin = join(afspraakJoin, MammaAfspraak_.uitnodiging, JoinType.LEFT);
		var screeningrondeJoin = join(uitnodigingJoin, MammaUitnodiging_.screeningRonde, JoinType.LEFT);
		return join(screeningrondeJoin, MammaScreeningRonde_.dossier, JoinType.LEFT);
	}

	private Join<MammaAfspraak, MammaOpkomstkans> opkomstkansJoin(Join<MammaCapaciteitBlok, MammaAfspraak> afspraakJoin)
	{
		return join(afspraakJoin, MammaAfspraak_.opkomstkans, JoinType.LEFT);
	}
}
