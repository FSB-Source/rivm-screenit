package nl.rivm.screenit.mamma.planning.repository;

/*-
 * ========================LICENSE_START=================================
 * screenit-planning-bk
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

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;

import javax.persistence.criteria.Join;
import javax.persistence.criteria.Root;

import nl.rivm.screenit.mamma.planning.repository.projectie.ClientProjectie;
import nl.rivm.screenit.model.BagAdres_;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.GbaPersoon_;
import nl.rivm.screenit.model.Gemeente_;
import nl.rivm.screenit.model.ScreeningRonde_;
import nl.rivm.screenit.model.SingleTableHibernateObject_;
import nl.rivm.screenit.model.TablePerClassHibernateObject_;
import nl.rivm.screenit.model.Uitnodiging_;
import nl.rivm.screenit.model.mamma.MammaAfspraak_;
import nl.rivm.screenit.model.mamma.MammaDeelnamekans_;
import nl.rivm.screenit.model.mamma.MammaDossier_;
import nl.rivm.screenit.model.mamma.MammaKansberekeningEvent_;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde_;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode_;
import nl.rivm.screenit.model.mamma.MammaUitnodiging_;
import nl.rivm.screenit.model.mamma.MammaUitnodigingsinterval_;
import nl.rivm.screenit.model.mamma.MammaUitstel;
import nl.rivm.screenit.model.mamma.MammaUitstel_;
import nl.rivm.screenit.model.mamma.MammaVolgendeUitnodiging_;
import nl.rivm.screenit.repository.BaseJpaRepository;
import nl.rivm.screenit.specification.HibernateObjectSpecification;
import nl.rivm.screenit.specification.algemeen.ClientSpecification;
import nl.rivm.screenit.specification.algemeen.PersoonSpecification;
import nl.rivm.screenit.specification.algemeen.ScreeningRondeSpecification;
import nl.rivm.screenit.specification.mamma.MammaUitstelSpecification;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject_;
import nl.topicuszorg.organisatie.model.Adres_;

import com.google.common.collect.Range;

import static javax.persistence.criteria.JoinType.LEFT;
import static nl.rivm.screenit.mamma.planning.model.PlanningConstanten.plannenTotEnMetGeboortedatum;
import static nl.rivm.screenit.mamma.planning.model.PlanningConstanten.plannenVanafGeboortedatum;
import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static org.springframework.data.jpa.domain.Specification.not;

public interface PlanningClientRepository extends BaseJpaRepository<Client>
{
	default List<ClientProjectie> findClientenVoorConceptmodel()
	{
		var specs = ClientSpecification.voldoetAanMammaClientSelectieRestricties()
			.and(PersoonSpecification.heeftGeboortedatumIn(Range.closed(plannenVanafGeboortedatum, plannenTotEnMetGeboortedatum)).with(Client_.persoon)
				.or(HibernateObjectSpecification.heeftId().withRoot(this::getLaatsteUitstelJoin)
					.and(not(MammaUitstelSpecification.heeftUitnodiging().withRoot(this::getLaatsteUitstelJoin)))
					.and(ScreeningRondeSpecification.isLopend().withRoot(this::getLaatsteScreeningRonde))));

		return findWith(specs, ClientProjectie.class, q -> q.projections((cb, r) ->
			{
				var dossierJoin = join(r, Client_.mammaDossier);
				var deelnamekansJoin = join(dossierJoin, MammaDossier_.deelnamekans);
				var persoonJoin = join(r, Client_.persoon);
				var adresJoin = join(persoonJoin, GbaPersoon_.gbaAdres);
				var tijdelijkGbaAdresJoin = join(persoonJoin, GbaPersoon_.tijdelijkGbaAdres, LEFT);
				var gemeenteJoin = join(adresJoin, BagAdres_.gbaGemeente);
				var screeningOrganisatieJoin = join(gemeenteJoin, Gemeente_.screeningOrganisatie);
				var laatsteScreeningRondeJoin = join(dossierJoin, MammaDossier_.laatsteScreeningRonde, LEFT);
				var laatsteUitstelJoin = join(laatsteScreeningRondeJoin, MammaScreeningRonde_.laatsteUitstel, LEFT)
					.on(cb.isNull(laatsteScreeningRondeJoin.get(MammaScreeningRonde_.laatsteUitstel).get(MammaUitstel_.geannuleerdOp)));
				var laatsteUitnodigingJoin = join(laatsteScreeningRondeJoin, MammaScreeningRonde_.laatsteUitnodiging, LEFT);
				var laatsteAfspraakJoin = join(laatsteUitnodigingJoin, MammaUitnodiging_.laatsteAfspraak, LEFT);
				var laatsteAfspraakStandplaatsPeriodeJoin = join(laatsteAfspraakJoin, MammaAfspraak_.standplaatsPeriode, LEFT);
				var screeningRondeEventJoin = join(dossierJoin, MammaDossier_.screeningRondeEvent, LEFT);
				var volgendeUitnodigingJoin = join(dossierJoin, MammaDossier_.volgendeUitnodiging, LEFT);
				var uitnodigingsIntervalJoin = join(volgendeUitnodigingJoin, MammaVolgendeUitnodiging_.interval, LEFT);

				return List.of(
					r.get(SingleTableHibernateObject_.id).alias("id"),
					persoonJoin.get(GbaPersoon_.geboortedatum).as(LocalDate.class).alias("geboortedatum"),
					adresJoin.get(Adres_.postcode).alias("postcode"),
					tijdelijkGbaAdresJoin.get(Adres_.postcode).alias("tijdelijkGbaPostcode"),
					screeningOrganisatieJoin.get(SingleTableHibernateObject_.id).alias("screeningOrgansatieId"),
					dossierJoin.get(TablePerClassHibernateObject_.id).alias("dossierId"),
					dossierJoin.get(MammaDossier_.doelgroep).alias("doelgroep"),
					dossierJoin.get(MammaDossier_.tehuis).get(AbstractHibernateObject_.id).alias("tehuisId"),
					dossierJoin.get(MammaDossier_.eersteOnderzoek).alias("eersteOnderzoek"),
					dossierJoin.get(MammaDossier_.laatsteMammografieAfgerond).as(LocalDateTime.class).alias("laatsteMammografieAfgerondOp"),
					deelnamekansJoin.get(MammaDeelnamekans_.deelnamekans).alias("deelnamekans"),
					laatsteScreeningRondeJoin.get(ScreeningRonde_.creatieDatum).as(LocalDateTime.class).alias("screeningRondeCreatieDatum"),
					laatsteScreeningRondeJoin.get(MammaScreeningRonde_.standplaatsRonde).get(AbstractHibernateObject_.id).alias("oorspronkelijkeStandplaatsRondeId"),
					laatsteScreeningRondeJoin.get(MammaScreeningRonde_.isGeforceerd).alias("screeningRondeIsGeforceerd"),
					laatsteUitstelJoin.get(MammaUitstel_.standplaats).get(AbstractHibernateObject_.id).alias("uitstelStandplaatsId"),
					laatsteUitstelJoin.get(MammaUitstel_.streefDatum).as(LocalDate.class).alias("uitstelStreefDatum"),
					laatsteUitstelJoin.get(MammaUitstel_.uitstelReden).alias("uitstelReden"),
					laatsteUitstelJoin.get(MammaUitstel_.uitnodiging).get(TablePerClassHibernateObject_.id).alias("uitstelUitnodigingId"),
					laatsteUitnodigingJoin.get(MammaUitnodiging_.standplaatsRonde).get(AbstractHibernateObject_.id).alias("uitnodigingStandplaatsRondeId"),
					laatsteAfspraakStandplaatsPeriodeJoin.get(MammaStandplaatsPeriode_.standplaatsRonde).get(AbstractHibernateObject_.id).alias("afspraakStandplaatsRondeId"),
					laatsteAfspraakJoin.get(MammaAfspraak_.afgezegdOp).as(LocalDateTime.class).alias("afspraakAfgezegdOp"),
					screeningRondeEventJoin.get(MammaKansberekeningEvent_.voorgaandeScreeningRondes).alias("voorgaandeScreeningRondes"),
					laatsteUitnodigingJoin.get(Uitnodiging_.creatieDatum).as(LocalDateTime.class).alias("laatsteUitnodigingDatum"),
					laatsteAfspraakJoin.get(MammaAfspraak_.status).alias("afspraakStatus"),
					laatsteAfspraakJoin.get(MammaAfspraak_.vanaf).as(LocalDateTime.class).alias("afspraakMoment"),
					uitnodigingsIntervalJoin.get(MammaUitnodigingsinterval_.type).alias("uitnodigingsIntervalType"));
			})
			.all());
	}

	private Join<?, MammaUitstel> getLaatsteUitstelJoin(Root<Client> r)
	{
		var dossierJoin = join(r, Client_.mammaDossier);
		var laatsteScreeningRondeJoin = join(dossierJoin, MammaDossier_.laatsteScreeningRonde, LEFT);
		return join(laatsteScreeningRondeJoin, MammaScreeningRonde_.laatsteUitstel, LEFT);
	}

	private Join<?, MammaScreeningRonde> getLaatsteScreeningRonde(Root<Client> r)
	{
		var dossierJoin = join(r, Client_.mammaDossier);
		return join(dossierJoin, MammaDossier_.laatsteScreeningRonde, LEFT);
	}
}
