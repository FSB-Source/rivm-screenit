package nl.rivm.screenit.specification.colon;

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

import java.time.LocalDate;
import java.util.List;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.From;
import javax.persistence.criteria.Join;
import javax.persistence.criteria.JoinType;
import javax.persistence.criteria.Order;
import javax.persistence.criteria.Root;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.BagAdres;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.GbaPersoon_;
import nl.rivm.screenit.model.ScreeningRonde_;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.colon.ColonDossier_;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.ColonScreeningRonde_;
import nl.rivm.screenit.model.colon.ColonUitnodiging;
import nl.rivm.screenit.model.colon.ColonVolgendeUitnodiging;
import nl.rivm.screenit.model.colon.IFOBTTest;
import nl.rivm.screenit.model.colon.IFOBTTest_;
import nl.rivm.screenit.model.colon.UitnodigingsGebied;
import nl.rivm.screenit.model.colon.enums.IFOBTTestStatus;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.project.ProjectClient;
import nl.rivm.screenit.specification.ExtendedSpecification;
import nl.rivm.screenit.specification.algemeen.ScreeningRondeSpecification;

import org.jetbrains.annotations.NotNull;

import static javax.persistence.criteria.JoinType.INNER;
import static javax.persistence.criteria.JoinType.LEFT;
import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.algemeen.BagAdresSpecification.valtBinnen;
import static nl.rivm.screenit.specification.algemeen.ClientSpecification.heeftActieveClient;
import static nl.rivm.screenit.specification.algemeen.ClientSpecification.heeftGeenActieveProjectClienten;
import static nl.rivm.screenit.specification.algemeen.DossierSpecification.isAangemeld;
import static nl.rivm.screenit.specification.algemeen.DossierSpecification.wachtOpStartProject;
import static nl.rivm.screenit.specification.algemeen.PersoonSpecification.heeftGeboorteJaarIn;
import static nl.rivm.screenit.specification.algemeen.PersoonSpecification.valtBinnenLeeftijdGrensRestricties;
import static nl.rivm.screenit.specification.algemeen.ProjectClientSpecification.heeftActieveProjectClient;
import static nl.rivm.screenit.specification.colon.ColonDossierSpecification.heeftGeenGevuldDossier;
import static nl.rivm.screenit.specification.colon.ColonIntakeAfspraakSpecification.heeftGeenAfspraakVanaf;
import static nl.rivm.screenit.specification.colon.ColonUitnodigingSpecification.heeftGeenUitnodiging;
import static nl.rivm.screenit.specification.colon.ColonUitnodigingSpecification.isVerstuurd;
import static nl.rivm.screenit.specification.colon.ColonVolgendeUitnodigingSpecification.getReferentieSpecification;
import static nl.rivm.screenit.specification.colon.ColonVolgendeUitnodigingSpecification.heeftGeenId;
import static org.apache.commons.collections4.CollectionUtils.isNotEmpty;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class ColonUitnodigingBaseSpecification
{
	public static ExtendedSpecification<ColonDossier> u2Base(LocalDate peildatum, LocalDate vandaag, JoinType referentieJoinType)
	{
		return (r, q, cb) ->
			getReferentieSpecification(peildatum, vandaag, referentieJoinType).with(ColonDossier_.volgendeUitnodiging, referentieJoinType)
				.and(isAangemeld(true))
				.and(heeftGeenAfspraakVanaf(peildatum).with(root -> laatsteAfspraakJoin(r, referentieJoinType))).toPredicate(r, q, cb);
	}

	public static ExtendedSpecification<Client> u1Base(LocalDate peilDatum, LocalDate vandaag, List<Integer> geboorteJaren)
	{
		return (r, q, cb) ->
			heeftGeenGevuldDossier().with(root -> dossierJoin(r, LEFT))
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

	public static ExtendedSpecification<Client> getSpecificationU1(Integer minimaleLeeftijd, Integer maximaleLeeftijd, LocalDate peildatum,
		UitnodigingsGebied uitnodigingsgebied, List<Integer> geboortejaren, Long projectGroupId, List<Long> exclusieGroepIds, LocalDate vandaag)
	{
		var specification = clientUitnodigingBase(minimaleLeeftijd, maximaleLeeftijd, peildatum, uitnodigingsgebied)
			.and(u1Base(vandaag, vandaag, geboortejaren))
			.and(wachtOpStartProject(Boolean.FALSE).with(r -> dossierJoin(r, LEFT)));

		return addProjectClientSpecifications(projectGroupId, exclusieGroepIds, specification);
	}

	public static ExtendedSpecification<Client> getSpecificationU2(Integer minimaleLeeftijd, Integer maximaleLeeftijd, LocalDate peildatum,
		UitnodigingsGebied uitnodigingsgebied, Long projectGroupId, List<Long> exclusieGroepIds, LocalDate vandaag)
	{
		var specification = clientUitnodigingBase(minimaleLeeftijd, maximaleLeeftijd, peildatum, uitnodigingsgebied)
			.and(wachtOpStartProject(Boolean.FALSE).with(r -> dossierJoin(r, INNER)))
			.and(heeftGeenUitnodiging().or(isVerstuurd()).with(r -> laatsteUitnodigingJoin(r)))
			.and(u2Base(vandaag, vandaag, JoinType.INNER).with(r -> dossierJoin(r, INNER)));

		return addProjectClientSpecifications(projectGroupId, exclusieGroepIds, specification);
	}

	public static @NotNull List<Order> getU1OrderByList(Root<Client> r, CriteriaBuilder cb)
	{
		return List.of(cb.asc(persoonJoin(r).get(GbaPersoon_.geboortedatum)), cb.asc(persoonJoin(r).get(GbaPersoon_.achternaam)));
	}

	public static @NotNull List<Order> getU2OrderByList(Root<Client> r, CriteriaBuilder cb)
	{
		return List.of(
			cb.asc(laatsteScreeningRondeJoin(r, INNER).get(ScreeningRonde_.creatieDatum)),
			cb.asc(persoonJoin(r).get(GbaPersoon_.geboortedatum)),
			cb.asc(persoonJoin(r).get(GbaPersoon_.achternaam)));
	}

	public static ExtendedSpecification<Client> getSpecificationU3(LocalDate peildatum)
	{
		return clientUitnodigingBase(null, null, peildatum, null)
			.and(heeftGeenUitnodiging().or(isVerstuurd()).with(r -> laatsteUitnodigingJoin(r)))
			.and(getFitStatusSpecifications(IFOBTTestStatus.NIETTEBEOORDELEN))
			.and(heeftColonUitslagBrieven());
	}

	public static ExtendedSpecification<Client> getSpecificationU4(LocalDate peildatum)
	{
		return clientUitnodigingBase(null, null, peildatum, null)
			.and(heeftGeenUitnodiging().or(isVerstuurd()).with(r -> laatsteUitnodigingJoin(r)))
			.and(getFitStatusSpecifications(IFOBTTestStatus.VERLOREN))
			.and(heeftColonUitslagBrieven())
			.and(ColonUitnodigingSpecification.heeftGeenRetourzendingReden().with(r -> fitUitnodigingJoin(r)))
			.and(ColonUitnodigingSpecification.heeftGeenRetourzendingReden().with(r -> fitExtraUitnodigingJoin(r)));
	}

	public static ExtendedSpecification<Client> getSpecificationU6(LocalDate peildatum)
	{
		return clientUitnodigingBase(null, null, peildatum, null)
			.and(heeftGeenUitnodiging().or(isVerstuurd()).with(r -> laatsteUitnodigingJoin(r)))
			.and(getFitStatusSpecifications(IFOBTTestStatus.VERVALDATUMVERLOPEN))
			.and(heeftColonUitslagBrieven());
	}

	private static ExtendedSpecification<Client> addProjectClientSpecifications(Long projectGroupId, List<Long> exclusieGroepIds, ExtendedSpecification<Client> specification)
	{
		if (projectGroupId != null)
		{
			specification = specification.and(heeftActieveProjectClient(projectGroupId).with(r -> projectClientJoin(r)));
		}
		else if (isNotEmpty(exclusieGroepIds))
		{
			specification = specification.and(heeftGeenActieveProjectClienten(exclusieGroepIds));
		}
		return specification;
	}

	private static ExtendedSpecification<Client> getFitStatusSpecifications(IFOBTTestStatus status)
	{
		return ScreeningRondeSpecification.isLopend().with((From<?, ? extends Client> r) -> laatsteScreeningRondeJoin(r, INNER))
			.and(ColonFITSpecification.heeftStatus(status).with(r -> laatsteFitJoin(r)))
			.and(ColonFITSpecification.heeftGeenStatus()
				.or(ColonFITSpecification.heeftStatusIn(List.of(IFOBTTestStatus.UNMUTABLE_EIND_STATUSSEN))).with(r -> laatsteFitExtraJoin(r)))
			;
	}

	private static ExtendedSpecification<Client> heeftColonUitslagBrieven()
	{
		return ColonScreeningRondeSpecification.heeftGeenBriefVanTypeIn(BriefType.COLON_UITSLAG_BRIEVEN).with(r -> laatsteScreeningRondeJoin(r, INNER));
	}

	private static Join<ColonScreeningRonde, ColonIntakeAfspraak> laatsteAfspraakJoin(From<?, ? extends ColonDossier> r, JoinType joinType)
	{
		var screeningRondeJoin = join(r, ColonDossier_.laatsteScreeningRonde, joinType);
		return join(screeningRondeJoin, ColonScreeningRonde_.laatsteAfspraak, LEFT);
	}

	public static Join<? extends ColonDossier, ColonScreeningRonde> laatsteScreeningRondeJoin(From<?, ? extends Client> r, JoinType joinType)
	{
		var dossierJoin = dossierJoin(r, joinType);
		return join(dossierJoin, ColonDossier_.laatsteScreeningRonde, joinType);
	}

	private static Join<? extends ColonDossier, ColonVolgendeUitnodiging> volgendeUitnodigingJoin(From<?, ? extends Client> r)
	{
		return join(dossierJoin(r, LEFT), ColonDossier_.volgendeUitnodiging, LEFT);
	}

	private static Join<? extends Client, ColonDossier> dossierJoin(From<?, ? extends Client> r, JoinType joinType)
	{
		return join(r, Client_.colonDossier, joinType);
	}

	private static Join<?, ? extends GbaPersoon> persoonJoin(From<?, ? extends Client> r)
	{
		return join(r, Client_.persoon);
	}

	private static Join<?, ? extends ProjectClient> projectClientJoin(From<?, ? extends Client> r)
	{
		return join(r, Client_.projecten);
	}

	private static Join<?, ? extends BagAdres> adresJoin(From<?, ? extends Client> r)
	{
		return join(persoonJoin(r), GbaPersoon_.gbaAdres);
	}

	private static Join<?, ? extends ColonUitnodiging> laatsteUitnodigingJoin(From<?, ? extends Client> r)
	{
		var screeningRondeJoin = laatsteScreeningRondeJoin(r, INNER);
		return join(screeningRondeJoin, ColonScreeningRonde_.laatsteUitnodiging, LEFT);
	}

	private static Join<?, ? extends ColonUitnodiging> fitUitnodigingJoin(From<?, ? extends Client> r)
	{
		var laatsteFitJoin = laatsteFitJoin(r);
		return join(laatsteFitJoin, IFOBTTest_.colonUitnodiging, LEFT);
	}

	private static Join<?, ? extends ColonUitnodiging> fitExtraUitnodigingJoin(From<?, ? extends Client> r)
	{
		var laatsteFitJoin = laatsteFitExtraJoin(r);
		return join(laatsteFitJoin, IFOBTTest_.colonUitnodigingExtra, LEFT);
	}

	private static Join<ColonScreeningRonde, IFOBTTest> laatsteFitJoin(From<?, ? extends Client> r)
	{
		var screeningRondeJoin = laatsteScreeningRondeJoin(r, INNER);
		return join(screeningRondeJoin, ColonScreeningRonde_.laatsteIFOBTTest, LEFT);
	}

	private static Join<ColonScreeningRonde, IFOBTTest> laatsteFitExtraJoin(From<?, ? extends Client> r)
	{
		var screeningRondeJoin = laatsteScreeningRondeJoin(r, INNER);
		return join(screeningRondeJoin, ColonScreeningRonde_.laatsteIFOBTTestExtra, LEFT);
	}
}
