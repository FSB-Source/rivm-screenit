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

import java.util.Date;
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
import nl.rivm.screenit.model.cervix.CervixMonster_;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde_;
import nl.rivm.screenit.model.cervix.CervixUitnodiging_;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje_;
import nl.rivm.screenit.model.cervix.enums.CervixCytologieOrderStatus;
import nl.rivm.screenit.model.cervix.enums.CervixHpvBeoordelingWaarde;
import nl.rivm.screenit.model.cervix.enums.CervixHuisartsBerichtStatus;
import nl.rivm.screenit.model.cervix.enums.CervixLabformulierStatus;
import nl.rivm.screenit.model.cervix.enums.CervixUitstrijkjeStatus;
import nl.rivm.screenit.model.cervix.facturatie.CervixBoekRegel;
import nl.rivm.screenit.util.DateUtil;

import org.apache.commons.lang.StringUtils;
import org.springframework.data.jpa.domain.Specification;

import static nl.rivm.screenit.model.cervix.CervixLabformulierenFilter.LabprocesStap;
import static nl.rivm.screenit.model.cervix.CervixLabformulierenFilter.LabprocesStap.CONTROLEREN_VOOR_CYTOLOGIE;
import static nl.rivm.screenit.model.cervix.CervixLabformulierenFilter.LabprocesStap.CYTOLOGIE;
import static nl.rivm.screenit.model.cervix.CervixLabformulierenFilter.LabprocesStap.HUISARTS_ONBEKEND;
import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenEmpty;
import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenFalsy;
import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenNull;
import static nl.rivm.screenit.specification.cervix.CervixBoekRegelSpecification.labformulierJoin;

@AllArgsConstructor(access = AccessLevel.PRIVATE)

public class CervixLabformulierSpecification
{

	public static Specification<CervixLabformulier> filterHeeftOrganisatieType(OrganisatieType organisatieType, Long instellingId)
	{
		return (r, q, cb) ->
		{
			if (organisatieType == OrganisatieType.BMHK_LABORATORIUM)
			{
				return cb.equal(join(r, CervixLabformulier_.laboratorium).get(SingleTableHibernateObject_.id), instellingId);
			}
			else if (organisatieType == OrganisatieType.SCREENINGSORGANISATIE)
			{
				return cb.equal(screeningOrganisatieJoin(r).get(SingleTableHibernateObject_.id), instellingId);
			}
			else
			{
				return null;
			}
		};
	}

	public static Specification<CervixLabformulier> filterHeeftMonsterId(String monsterId)
	{
		return skipWhenNull(monsterId, (r, q, cb) -> cb.equal(r.get(ScannedFormulier_.barcode), monsterId));
	}

	public static Specification<CervixLabformulier> filterHeeftLabformulierStatussen(List<CervixLabformulierStatus> labformulierStatussen)
	{
		return skipWhenEmpty(labformulierStatussen, (r, q, cb) -> r.get(CervixLabformulier_.status).in(labformulierStatussen));
	}

	public static Specification<CervixLabformulier> filterHeeftScanDatumVanaf(Date scanDatumVanaf)
	{
		return skipWhenNull(scanDatumVanaf, (r, q, cb) -> cb.greaterThanOrEqualTo(r.get(ScannedFormulier_.scanDatum), scanDatumVanaf));
	}

	public static Specification<CervixLabformulier> filterHeeftScanDatumTotEnMet(Date scanDatumTotEnMet)
	{
		return skipWhenNull(scanDatumTotEnMet,
			(r, q, cb) -> cb.lessThanOrEqualTo(r.get(ScannedFormulier_.scanDatum), DateUtil.eindDag(scanDatumTotEnMet)));
	}

	public static Specification<CervixLabformulier> filterHeeftGeboortedatum(Date geboorteDatum)
	{
		return skipWhenNull(geboorteDatum, (r, q, cb) -> cb.equal(persoonJoin(r, JoinType.INNER).get(GbaPersoon_.geboortedatum), geboorteDatum));
	}

	public static Specification<CervixLabformulier> heeftGeldigHuisartsbericht(LabprocesStap labprocesStap, String bsn)
	{
		return (r, q, cb) ->
		{
			var joinType = getJoinTypeVanLabprocesStap(labprocesStap, bsn);
			var uitstrijkjeJoin = join(r, CervixLabformulier_.uitstrijkje, joinType);
			var huisartsBerichtJoin = join(uitstrijkjeJoin, CervixUitstrijkje_.huisartsBericht, JoinType.LEFT);
			return cb.and(
				cb.or(cb.isNull(huisartsBerichtJoin.get(CervixHuisartsBericht_.status)), cb.notEqual(huisartsBerichtJoin.get(CervixHuisartsBericht_.status),
					CervixHuisartsBerichtStatus.VERSTUURD)),
				cb.or(cb.isNull(huisartsBerichtJoin.get(CervixHuisartsBericht_.status)), cb.notEqual(huisartsBerichtJoin.get(CervixHuisartsBericht_.status),
					CervixHuisartsBerichtStatus.HUISARTS_ONBEKEND), cb.equal(r.get(CervixLabformulier_.status), CervixLabformulierStatus.AFGEKEURD)));
		};
	}

	public static Specification<CervixLabformulier> filterLabProcesStapIsHuisartsOnbekendOfControlerenVoorCytologie(LabprocesStap labprocesStap)
	{
		return skipWhenFalsy(labprocesStap == HUISARTS_ONBEKEND || labprocesStap == CONTROLEREN_VOOR_CYTOLOGIE, (r, q, cb) ->
		{
			var uitstrijkjeJoin = join(r, CervixLabformulier_.uitstrijkje);
			var screeningRondeJoin = join(uitstrijkjeJoin, CervixMonster_.ontvangstScreeningRonde);
			var monsterJoin = join(screeningRondeJoin, CervixScreeningRonde_.monsterHpvUitslag);
			var hpvBeoordelingJoin = join(monsterJoin, CervixMonster_.laatsteHpvBeoordeling);
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

	public static Specification<CervixLabformulier> filterLabProcesStapIsHuisartsOnbekend(LabprocesStap labprocesStap)
	{
		return skipWhenFalsy(labprocesStap == HUISARTS_ONBEKEND, (r, q, cb) ->
		{
			var huisartsOnbekendBriefJoin = join(r, CervixLabformulier_.huisartsOnbekendBrief);
			var mergedBrievenJoin = join(huisartsOnbekendBriefJoin, CervixBrief_.mergedBrieven);
			return cb.equal(mergedBrievenJoin.get(MergedBrieven_.geprint), true);
		});
	}

	public static Specification<CervixLabformulier> filterLabProcesStapIsControlerenVoorCytologie(LabprocesStap labprocesStap)
	{
		return skipWhenFalsy(labprocesStap == CONTROLEREN_VOOR_CYTOLOGIE, (r, q, cb) ->
		{
			var uitstrijkjeJoin = join(r, CervixLabformulier_.uitstrijkje);
			return cb.and(cb.isNull(uitstrijkjeJoin.get(CervixUitstrijkje_.cytologieOrder)),
				cb.isNull(uitstrijkjeJoin.get(CervixUitstrijkje_.huisartsBericht)));
		});
	}

	public static Specification<CervixLabformulier> filterLabProcesStapIsCytopathologie(LabprocesStap labprocesStap)
	{
		return skipWhenFalsy(labprocesStap == CYTOLOGIE, (r, q, cb) ->
		{
			var uitstrijkjeJoin = join(r, CervixLabformulier_.uitstrijkje);
			var cytologieOrderJoin = join(uitstrijkjeJoin, CervixUitstrijkje_.cytologieOrder);
			return cb.equal(cytologieOrderJoin.get(CervixCytologieOrder_.status), CervixCytologieOrderStatus.VERSTUURD);
		});
	}

	public static Specification<CervixLabformulier> filterBsnCheck(LabprocesStap labprocesStap, String bsn)
	{
		return skipWhenEmpty(bsn, (r, q, cb) ->
		{
			var joinType = getJoinTypeVanLabprocesStap(labprocesStap, bsn);
			return cb.equal(persoonJoin(r, joinType).get(GbaPersoon_.bsn), bsn);
		});
	}

	public static Specification<CervixLabformulier> filterOrganisatieTypeIsScreeningorganisatie(OrganisatieType organisatieType, Long instellingId)
	{
		return skipWhenFalsy(organisatieType == OrganisatieType.SCREENINGSORGANISATIE,
			(r, q, cb) -> cb.and(cb.equal(screeningOrganisatieJoin(r).get(SingleTableHibernateObject_.id), instellingId)));
	}

	public static Specification<CervixLabformulier> filterHeeftDigitaal(Boolean heeftDigitaal)
	{
		return skipWhenNull(heeftDigitaal, (r, q, cb) -> cb.equal(r.get(CervixLabformulier_.digitaal), heeftDigitaal));
	}

	public static Specification<CervixLabformulier> heeftStatussen(List<CervixUitstrijkjeStatus> statussen)
	{
		return (r, q, cb) ->
		{
			var uitstrijkjeJoin = r.get(CervixLabformulier_.uitstrijkje);
			return uitstrijkjeJoin.get(CervixUitstrijkje_.uitstrijkjeStatus).in(statussen);
		};
	}

	private static JoinType getJoinTypeVanLabprocesStap(LabprocesStap labprocesStap, String bsn)
	{
		if (labprocesStap == HUISARTS_ONBEKEND ||
			labprocesStap == CONTROLEREN_VOOR_CYTOLOGIE ||
			StringUtils.isNotBlank(bsn))
		{
			return JoinType.INNER;
		}
		else
		{
			return JoinType.LEFT;
		}
	}

	public static From<Client, GbaPersoon> persoonJoin(Root<CervixLabformulier> labformulierRoot, JoinType joinType)
	{
		var uitstrijkjeJoin = join(labformulierRoot, CervixLabformulier_.uitstrijkje, joinType);
		var uitnodigingJoin = join(uitstrijkjeJoin, CervixMonster_.uitnodiging, joinType);
		var screeningRondeJoin = join(uitnodigingJoin, CervixUitnodiging_.screeningRonde, joinType);
		var dossierJoin = join(screeningRondeJoin, CervixScreeningRonde_.dossier, joinType);
		var clientJoin = join(dossierJoin, CervixDossier_.client, joinType);
		return join(clientJoin, Client_.persoon, joinType);
	}

	private static From<Gemeente, ScreeningOrganisatie> screeningOrganisatieJoin(Root<CervixLabformulier> labformulierRoot)
	{
		var persoonJoin = persoonJoin(labformulierRoot, JoinType.INNER);
		var adresJoin = join(persoonJoin, GbaPersoon_.gbaAdres);
		var gemeenteJoin = join(adresJoin, BagAdres_.gbaGemeente);
		return join(gemeenteJoin, Gemeente_.screeningOrganisatie);
	}

	public static Specification<CervixBoekRegel> filterDatumUitstrijkje(Date datumUitstrijkje)
	{
		return skipWhenNull(datumUitstrijkje, (r, q, cb) -> cb.equal(labformulierJoin(r, cb, JoinType.INNER).get(CervixLabformulier_.datumUitstrijkje), datumUitstrijkje));
	}
}
