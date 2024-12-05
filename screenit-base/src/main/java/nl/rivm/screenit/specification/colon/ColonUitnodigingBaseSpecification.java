package nl.rivm.screenit.specification.colon;

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

import javax.persistence.criteria.From;
import javax.persistence.criteria.Join;
import javax.persistence.criteria.JoinType;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;

import nl.rivm.screenit.model.BagAdres;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.GbaPersoon_;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.colon.ColonDossier_;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.ColonScreeningRonde_;
import nl.rivm.screenit.model.colon.ColonVolgendeUitnodiging;
import nl.rivm.screenit.model.colon.UitnodigingsGebied;
import nl.rivm.screenit.specification.ExtendedSpecification;

import static javax.persistence.criteria.JoinType.LEFT;
import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.algemeen.BagAdresSpecification.valtBinnen;
import static nl.rivm.screenit.specification.algemeen.ClientSpecification.heeftActieveClient;
import static nl.rivm.screenit.specification.algemeen.DossierSpecification.isAangemeld;
import static nl.rivm.screenit.specification.algemeen.PersoonSpecification.heeftGeboorteJaarIn;
import static nl.rivm.screenit.specification.algemeen.PersoonSpecification.valtBinnenLeeftijdGrensRestricties;
import static nl.rivm.screenit.specification.colon.ColonDossierSpecification.heeftGeenGevuldDossier;
import static nl.rivm.screenit.specification.colon.ColonIntakeAfspraakSpecification.heeftGeenAfspraakVanaf;
import static nl.rivm.screenit.specification.colon.ColonVolgendeUitnodigingSpecification.getReferentieSpecification;
import static nl.rivm.screenit.specification.colon.ColonVolgendeUitnodigingSpecification.heeftGeenId;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class ColonUitnodigingBaseSpecification
{
	public static ExtendedSpecification<ColonDossier> u2Base(LocalDate peildatum, LocalDate vandaag, JoinType referentieJoinType)
	{
		return (r, q, cb) ->
			getReferentieSpecification(peildatum, vandaag, referentieJoinType).with(ColonDossier_.volgendeUitnodiging, referentieJoinType)
				.and(isAangemeld(true))
				.and(heeftGeenAfspraakVanaf(peildatum).with(root -> laatsteAfspraakJoin(r))).toPredicate(r, q, cb);
	}

	public static ExtendedSpecification<Client> u1Base(LocalDate peilDatum, LocalDate vandaag, List<Integer> geboorteJaren)
	{
		return (r, q, cb) ->
			heeftGeenGevuldDossier().with(root -> dossierJoin(r))
				.and(heeftGeenId().with(root -> volgendeUitnodigingJoin(r))
					.or(getReferentieSpecification(peilDatum, vandaag, LEFT).with(root -> volgendeUitnodigingJoin(r))))
				.and(heeftGeboorteJaarIn(geboorteJaren).with(root -> persoonJoin(r)))
				.toPredicate(r, q, cb);
	}

	public static ExtendedSpecification<Client> clientUitnodigingBase(Integer minimaleLeeftijd, Integer maximaleLeeftijd, LocalDate peilDatum,
		UitnodigingsGebied uitnodigingsgebied)
	{
		return (r, q, cb) ->
			heeftActieveClient()
				.and(valtBinnenLeeftijdGrensRestricties(minimaleLeeftijd, maximaleLeeftijd, null, peilDatum).with(root -> persoonJoin(r)))
				.and(valtBinnen(uitnodigingsgebied).with(root -> adresJoin(r)))
				.toPredicate(r, q, cb);
	}

	private static Join<ColonScreeningRonde, ColonIntakeAfspraak> laatsteAfspraakJoin(From<?, ? extends ColonDossier> r)
	{
		return join(laatsteScreeningRondeJoin(r), ColonScreeningRonde_.laatsteAfspraak, JoinType.LEFT);
	}

	private static Join<? extends ColonDossier, ColonScreeningRonde> laatsteScreeningRondeJoin(From<?, ? extends ColonDossier> r)
	{
		return join(r, ColonDossier_.laatsteScreeningRonde, JoinType.LEFT);
	}

	private static Join<? extends ColonDossier, ColonVolgendeUitnodiging> volgendeUitnodigingJoin(From<?, ? extends Client> r)
	{
		return join(dossierJoin(r), ColonDossier_.volgendeUitnodiging, LEFT);
	}

	private static Join<?, ? extends ColonDossier> dossierJoin(From<?, ? extends Client> r)
	{
		return join(r, Client_.colonDossier, LEFT);
	}

	private static Join<?, ? extends GbaPersoon> persoonJoin(From<?, ? extends Client> r)
	{
		return join(r, Client_.persoon);
	}

	private static Join<?, ? extends BagAdres> adresJoin(From<?, ? extends Client> r)
	{
		return join(persoonJoin(r), GbaPersoon_.gbaAdres);
	}

}
