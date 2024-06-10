package nl.rivm.screenit.specification.cervix;

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

import javax.persistence.criteria.From;
import javax.persistence.criteria.JoinType;
import javax.persistence.criteria.Root;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;

import nl.rivm.screenit.model.BagAdres_;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.GbaPersoon_;
import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.Gemeente_;
import nl.rivm.screenit.model.MergedBrieven_;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.ScannedFormulier_;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.SingleTableHibernateObject_;
import nl.rivm.screenit.model.cervix.CervixBrief_;
import nl.rivm.screenit.model.cervix.CervixCytologieOrder_;
import nl.rivm.screenit.model.cervix.CervixDossier_;
import nl.rivm.screenit.model.cervix.CervixHpvBeoordeling_;
import nl.rivm.screenit.model.cervix.CervixHuisartsBericht_;
import nl.rivm.screenit.model.cervix.CervixLabformulier;
import nl.rivm.screenit.model.cervix.CervixLabformulier_;
import nl.rivm.screenit.model.cervix.CervixLabformulierenFilter;
import nl.rivm.screenit.model.cervix.CervixMonster_;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde_;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje_;
import nl.rivm.screenit.model.cervix.enums.CervixCytologieOrderStatus;
import nl.rivm.screenit.model.cervix.enums.CervixHpvBeoordelingWaarde;
import nl.rivm.screenit.model.cervix.enums.CervixHuisartsBerichtStatus;
import nl.rivm.screenit.model.cervix.enums.CervixLabformulierStatus;
import nl.rivm.screenit.model.cervix.enums.CervixUitstrijkjeStatus;
import nl.rivm.screenit.specification.SpecificationUtil;
import nl.rivm.screenit.util.DateUtil;

import org.springframework.data.jpa.domain.Specification;

import static nl.rivm.screenit.model.cervix.CervixLabformulierenFilter.LabprocesStap.CONTROLEREN_VOOR_CYTOLOGIE;
import static nl.rivm.screenit.model.cervix.CervixLabformulierenFilter.LabprocesStap.CYTOLOGIE;
import static nl.rivm.screenit.model.cervix.CervixLabformulierenFilter.LabprocesStap.HUISARTS_ONBEKEND;
import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenFalsy;
import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenNull;

@AllArgsConstructor(access = AccessLevel.PRIVATE)

public class CervixLabformulierSpecification
{

	public static Specification<CervixLabformulier> filterHeeftOrganisatieType(CervixLabformulierenFilter filter)
	{
		return (r, q, cb) ->
		{
			if (filter.getOrganisatieType() == OrganisatieType.BMHK_LABORATORIUM)
			{
				return cb.equal(SpecificationUtil.join(r, CervixLabformulier_.laboratorium).get(SingleTableHibernateObject_.id), filter.getInstellingId());
			}
			else if (filter.getOrganisatieType() == OrganisatieType.SCREENINGSORGANISATIE)
			{
				return cb.equal(screeningOrganisatieJoin(r).get(SingleTableHibernateObject_.id), filter.getInstellingId());
			}
			else
			{
				return null;
			}
		};
	}

	public static Specification<CervixLabformulier> filterHeeftMonsterId(CervixLabformulierenFilter filter)
	{
		return skipWhenNull(filter.getMonsterId(), (r, q, cb) -> cb.equal(r.get(ScannedFormulier_.barcode), filter.getMonsterId()));
	}

	public static Specification<CervixLabformulier> filterHeeftLabformulierStatussen(CervixLabformulierenFilter filter)
	{
		return (r, q, cb) ->
		{
			if (!filter.getLabformulierStatussen().isEmpty())
			{
				return r.get(CervixLabformulier_.status).in(filter.getLabformulierStatussen());
			}
			else
			{
				return cb.disjunction();
			}
		};
	}

	public static Specification<CervixLabformulier> filterHeeftScanDatumVanaf(CervixLabformulierenFilter filter)
	{
		return skipWhenNull(filter.getScanDatumVanaf(), (r, q, cb) -> cb.greaterThanOrEqualTo(r.get(ScannedFormulier_.scanDatum), filter.getScanDatumVanaf()));
	}

	public static Specification<CervixLabformulier> filterHeeftScanDatumTotEnMet(CervixLabformulierenFilter filter)
	{
		return skipWhenNull(filter.getScanDatumTotEnMet(),
			(r, q, cb) -> cb.lessThanOrEqualTo(r.get(ScannedFormulier_.scanDatum), DateUtil.eindDag(filter.getScanDatumTotEnMet())));
	}

	public static Specification<CervixLabformulier> filterHeeftGeboortedatum(CervixLabformulierenFilter filter)
	{
		return skipWhenNull(filter.getGeboortedatum(), (r, q, cb) -> cb.equal(persoonJoin(r).get(GbaPersoon_.geboortedatum), filter.getGeboortedatum()));
	}

	public static Specification<CervixLabformulier> heeftGeldigHuisartsbericht(CervixLabformulierenFilter filter)
	{
		return (r, q, cb) ->
		{
			var joinType = getJoinTypeVanLabprocesStap(filter);
			var uitstrijkjeJoin = SpecificationUtil.join(r, CervixLabformulier_.uitstrijkje, joinType);
			var huisartsBerichtJoin = SpecificationUtil.join(uitstrijkjeJoin, CervixUitstrijkje_.huisartsBericht, JoinType.LEFT);
			return cb.and(
				cb.or(cb.isNull(huisartsBerichtJoin.get(CervixHuisartsBericht_.status)), cb.notEqual(huisartsBerichtJoin.get(CervixHuisartsBericht_.status),
					CervixHuisartsBerichtStatus.VERSTUURD)),
				cb.or(cb.isNull(huisartsBerichtJoin.get(CervixHuisartsBericht_.status)), cb.notEqual(huisartsBerichtJoin.get(CervixHuisartsBericht_.status),
					CervixHuisartsBerichtStatus.HUISARTS_ONBEKEND), cb.equal(r.get(CervixLabformulier_.status), CervixLabformulierStatus.AFGEKEURD)));
		};
	}

	public static Specification<CervixLabformulier> filterLabProcesStapIsHuisartsOnbekendOfControlerenVoorCytologie(CervixLabformulierenFilter filter)
	{
		return skipWhenFalsy(filter.getLabprocesStap() == HUISARTS_ONBEKEND || filter.getLabprocesStap() == CONTROLEREN_VOOR_CYTOLOGIE, (r, q, cb) ->
		{
			var uitstrijkjeJoin = SpecificationUtil.join(r, CervixLabformulier_.uitstrijkje);
			var screeningRondeJoin = SpecificationUtil.join(uitstrijkjeJoin, CervixMonster_.ontvangstScreeningRonde);
			var monsterJoin = SpecificationUtil.join(screeningRondeJoin, CervixScreeningRonde_.monsterHpvUitslag);
			var hpvBeoordelingJoin = SpecificationUtil.join(monsterJoin, CervixMonster_.laatsteHpvBeoordeling);
			return cb.and(
				cb.equal(hpvBeoordelingJoin.get(CervixHpvBeoordeling_.hpvUitslag), CervixHpvBeoordelingWaarde.POSITIEF),
				heeftStatussen(List.of(CervixUitstrijkjeStatus.ONTVANGEN, CervixUitstrijkjeStatus.GEANALYSEERD_OP_HPV_POGING_1,
					CervixUitstrijkjeStatus.GEANALYSEERD_OP_HPV_POGING_2)).toPredicate(r, q, cb),
				cb.or(cb.isNull(screeningRondeJoin.get(CervixScreeningRonde_.uitstrijkjeCytologieUitslag)),
					cb.and(cb.isNotNull(screeningRondeJoin.get(CervixScreeningRonde_.inVervolgonderzoekDatum)),
						cb.greaterThanOrEqualTo(uitstrijkjeJoin.get(CervixMonster_.ontvangstdatum), screeningRondeJoin.get(CervixScreeningRonde_.inVervolgonderzoekDatum)),
						cb.isNull(screeningRondeJoin.get(CervixScreeningRonde_.uitstrijkjeVervolgonderzoekUitslag)))));
		});
	}

	public static Specification<CervixLabformulier> filterLabProcesStapIsHuisartsOnbekend(CervixLabformulierenFilter filter)
	{
		return skipWhenFalsy(filter.getLabprocesStap() == HUISARTS_ONBEKEND, (r, q, cb) ->
		{
			var huisartsOnbekendBriefJoin = SpecificationUtil.join(r, CervixLabformulier_.huisartsOnbekendBrief);
			var mergedBrievenJoin = SpecificationUtil.join(huisartsOnbekendBriefJoin, CervixBrief_.mergedBrieven);
			return cb.equal(mergedBrievenJoin.get(MergedBrieven_.geprint), true);
		});
	}

	public static Specification<CervixLabformulier> filterLabProcesStapIsControlerenVoorCytologie(CervixLabformulierenFilter filter)
	{
		return skipWhenFalsy(filter.getLabprocesStap() == CONTROLEREN_VOOR_CYTOLOGIE, (r, q, cb) ->
		{
			var uitstrijkjeJoin = SpecificationUtil.join(r, CervixLabformulier_.uitstrijkje);
			return cb.and(cb.isNull(uitstrijkjeJoin.get(CervixUitstrijkje_.cytologieOrder)),
				cb.isNull(uitstrijkjeJoin.get(CervixUitstrijkje_.huisartsBericht)));
		});
	}

	public static Specification<CervixLabformulier> filterLabProcesStapIsCytopathologie(CervixLabformulierenFilter filter)
	{
		return skipWhenFalsy(filter.getLabprocesStap() == CYTOLOGIE, (r, q, cb) ->
		{
			var uitstrijkjeJoin = SpecificationUtil.join(r, CervixLabformulier_.uitstrijkje);
			var cytologieOrderJoin = SpecificationUtil.join(uitstrijkjeJoin, CervixUitstrijkje_.cytologieOrder);
			return cb.equal(cytologieOrderJoin.get(CervixCytologieOrder_.status), CervixCytologieOrderStatus.VERSTUURD);
		});
	}

	public static Specification<CervixLabformulier> filterBsnCheck(CervixLabformulierenFilter filter)
	{
		return skipWhenNull(filter.getBsn(), (r, q, cb) ->
		{
			var joinType = getJoinTypeVanLabprocesStap(filter);
			var uitstrijkjeJoin = SpecificationUtil.join(r, CervixLabformulier_.uitstrijkje, joinType);
			var screeningRondeJoin = SpecificationUtil.join(uitstrijkjeJoin, CervixMonster_.ontvangstScreeningRonde, joinType);
			var dossierJoin = SpecificationUtil.join(screeningRondeJoin, CervixScreeningRonde_.dossier, joinType);
			var clientJoin = SpecificationUtil.join(dossierJoin, CervixDossier_.client, joinType);
			var persoonJoin = SpecificationUtil.join(clientJoin, Client_.persoon, joinType);
			return cb.equal(persoonJoin.get(GbaPersoon_.bsn), filter.getBsn());
		});
	}

	public static Specification<CervixLabformulier> filterOrganisatieTypeIsScreeningorganisatie(CervixLabformulierenFilter filter)
	{
		return skipWhenFalsy(filter.getOrganisatieType() == OrganisatieType.SCREENINGSORGANISATIE,
			(r, q, cb) -> cb.and(cb.equal(screeningOrganisatieJoin(r).get(SingleTableHibernateObject_.id), filter.getInstellingId())));
	}

	public static Specification<CervixLabformulier> filterHeeftDigitaal(CervixLabformulierenFilter filter)
	{
		return skipWhenNull(filter.getDigitaal(), (r, q, cb) -> cb.equal(r.get(CervixLabformulier_.digitaal), filter.getDigitaal()));
	}

	public static Specification<CervixLabformulier> heeftStatussen(List<CervixUitstrijkjeStatus> statussen)
	{
		return (r, q, cb) ->
		{
			var uitstrijkjeJoin = r.get(CervixLabformulier_.uitstrijkje);
			return uitstrijkjeJoin.get(CervixUitstrijkje_.uitstrijkjeStatus).in(statussen);
		};
	}

	private static JoinType getJoinTypeVanLabprocesStap(CervixLabformulierenFilter filter)
	{
		if (filter.getLabprocesStap() == HUISARTS_ONBEKEND ||
			filter.getLabprocesStap() == CONTROLEREN_VOOR_CYTOLOGIE ||
			filter.getBsn() != null)
		{
			return JoinType.INNER;
		}
		else
		{
			return JoinType.LEFT;
		}
	}

	private static From<Client, GbaPersoon> persoonJoin(Root<CervixLabformulier> labformulierRoot)
	{
		var uitstrijkjeJoin = SpecificationUtil.join(labformulierRoot, CervixLabformulier_.uitstrijkje);
		var screeningRondeJoin = SpecificationUtil.join(uitstrijkjeJoin, CervixMonster_.ontvangstScreeningRonde);
		var dossierJoin = SpecificationUtil.join(screeningRondeJoin, CervixScreeningRonde_.dossier);
		var clientJoin = SpecificationUtil.join(dossierJoin, CervixDossier_.client);
		return SpecificationUtil.join(clientJoin, Client_.persoon);
	}

	private static From<Gemeente, ScreeningOrganisatie> screeningOrganisatieJoin(Root<CervixLabformulier> labformulierRoot)
	{
		var persoonJoin = persoonJoin(labformulierRoot);
		var adresJoin = SpecificationUtil.join(persoonJoin, GbaPersoon_.gbaAdres);
		var gemeenteJoin = SpecificationUtil.join(adresJoin, BagAdres_.gbaGemeente);
		return SpecificationUtil.join(gemeenteJoin, Gemeente_.screeningOrganisatie);
	}
}
