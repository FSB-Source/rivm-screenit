package nl.rivm.screenit.util.cervix;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.math.BigDecimal;

import nl.rivm.screenit.huisartsenportaal.dto.AanvraagDto;
import nl.rivm.screenit.huisartsenportaal.dto.AdresDto;
import nl.rivm.screenit.huisartsenportaal.dto.BetalingDto;
import nl.rivm.screenit.huisartsenportaal.dto.HuisartsDto;
import nl.rivm.screenit.huisartsenportaal.dto.LocatieDto;
import nl.rivm.screenit.huisartsenportaal.dto.VerrichtingDto;
import nl.rivm.screenit.huisartsenportaal.dto.WoonplaatsDto;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.Woonplaats;
import nl.rivm.screenit.model.cervix.CervixHuisarts;
import nl.rivm.screenit.model.cervix.CervixHuisartsAdres;
import nl.rivm.screenit.model.cervix.CervixHuisartsLocatie;
import nl.rivm.screenit.model.cervix.CervixLabformulierAanvraag;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;
import nl.rivm.screenit.model.cervix.facturatie.CervixBoekRegel;
import nl.rivm.screenit.model.cervix.facturatie.CervixVerrichting;
import nl.rivm.screenit.util.NaamUtil;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CervixHuisartsToDtoUtil
{

	private static final Logger LOG = LoggerFactory.getLogger(CervixHuisartsToDtoUtil.class);

	private CervixHuisartsToDtoUtil()
	{
	}

	public static HuisartsDto getHuisartsDto(CervixHuisarts huisarts)
	{
		HuisartsDto huisartsDto = new HuisartsDto();
		huisartsDto.setScreenitId(huisarts.getScreenitId());
		huisartsDto.setHuisartsportaalId(huisarts.getHuisartsportaalId());
		huisartsDto.setAgbcode(huisarts.getAgbcode());
		huisartsDto.setEmail(huisarts.getEmail());
		Gebruiker gebruiker = huisarts.getOrganisatieMedewerkers().get(0).getMedewerker();
		huisartsDto.setAchternaam(gebruiker.getAchternaam());
		huisartsDto.setTussenvoegsel(gebruiker.getTussenvoegsel());
		huisartsDto.setVoorletters(gebruiker.getVoorletters());
		huisartsDto.setInlogCode(gebruiker.getWachtwoordChangeCode());
		huisartsDto.setAanmeldStatus(huisarts.getAanmeldStatus().name());
		huisartsDto.setTelefoon(huisarts.getTelefoon());
		if (gebruiker.getAanhef() != null)
		{
			huisartsDto.setAanhef(gebruiker.getAanhef().getNaam());
		}
		huisartsDto.setActief(huisarts.getActief());
		huisartsDto.setExtraEmails(huisarts.getExtraEmails());
		if (huisarts.getPostadres() != null)
		{
			huisartsDto.setPostadres(getAdresDto(huisarts.getPostadres()));
		}
		return huisartsDto;
	}

	public static LocatieDto getLocatieDto(CervixHuisartsLocatie locatie)
	{
		LocatieDto locatieDto = new LocatieDto();
		locatieDto.setScreenitId(locatie.getScreenitId());
		locatieDto.setHuisartsportaalId(locatie.getHuisartsportaalId());
		locatieDto.setIban(locatie.getIban());
		locatieDto.setIbanTenaamstelling(locatie.getIbanTenaamstelling());
		locatieDto.setZorgmailklantnummer(locatie.getZorgmailklantnummer());
		locatieDto.setNaam(locatie.getNaam());
		locatieDto.setHuisartsId(locatie.getHuisarts().getId());
		locatieDto.setStatus(locatie.getStatus().name());
		if (locatie.getLocatieAdres() != null)
		{
			locatieDto.setLocatieAdres(getAdresDto(locatie.getLocatieAdres()));
		}
		return locatieDto;
	}

	public static AanvraagDto getAanvraagDto(CervixLabformulierAanvraag aanvraag)
	{
		AanvraagDto aanvraagDto = new AanvraagDto();
		aanvraagDto.setScreenitId(aanvraag.getId());
		aanvraagDto.setHuisartsportaalId(aanvraag.getHuisartsportaalId());
		aanvraagDto.setAantal(aanvraag.getAantal());
		aanvraagDto.setStatus(aanvraag.getStatus().getNaam());
		aanvraagDto.setStatusDatum(aanvraag.getStatusDatum());
		aanvraagDto.setAanvraagDatum(aanvraag.getAanvraagDatum());
		aanvraagDto.setAangevraagdDoor(aanvraag.getInstellingGebruiker().getOrganisatie().getNaam());
		aanvraagDto.setLocatie(getLocatieDto(aanvraag.getHuisartsLocatie()));
		return aanvraagDto;
	}

	public static VerrichtingDto getVerrichtingDto(CervixVerrichting verrichting)
	{
		VerrichtingDto verrichtingDto = new VerrichtingDto();
		verrichtingDto.setScreenitId(verrichting.getId());
		verrichtingDto.setRegio(verrichting.getRegio().getNaam());
		verrichtingDto.setMonsterId(verrichting.getMonster().getMonsterId());
		verrichtingDto.setVerrichtingsDatum(verrichting.getVerrichtingsDatum());

		for (CervixBoekRegel cervixBoekRegel : verrichting.getBoekRegels())
		{
			BetalingDto betalingDto = getBetalingDto(cervixBoekRegel);
			verrichtingDto.getBetalingen().add(betalingDto);
		}

		CervixUitstrijkje uitstrijkje = (CervixUitstrijkje) HibernateHelper.deproxy(verrichting.getMonster());
		if (uitstrijkje.getLabformulier() != null)
		{
			verrichtingDto.setDatumUitstrijkje(uitstrijkje.getLabformulier().getDatumUitstrijkje());
			verrichtingDto.setFormulierOntvangstDatum(uitstrijkje.getLabformulier().getScanDatum());
		}
		else
		{
			LOG.error("Geen labformulier bekend bij aanmaken huisarts verrichting. monsterID: {}", verrichting.getMonster().getMonsterId());
		}
		verrichtingDto.setHuisartsLocatie(getLocatieDto(verrichting.getHuisartsLocatie()));
		verrichtingDto.setClientNaam(NaamUtil.getAchternaamVoorlettersTussenvoegsel(verrichting.getClient()));
		return verrichtingDto;
	}

	public static BetalingDto getBetalingDto(CervixBoekRegel boekRegel)
	{

		BigDecimal huisartsBedrag = CervixTariefUtil.getHuisartsBedrag(boekRegel);
		BetalingDto betalingDto = new BetalingDto();

		if (boekRegel.getDebet())
		{
			huisartsBedrag = huisartsBedrag.negate();
		}

		if (boekRegel.getSpecificatie() != null)
		{
			betalingDto.setBetalingsdatum(boekRegel.getSpecificatie().getBetaalopdrachtRegel().getBetaalopdracht().getStatusDatum());
			betalingDto.setBetalingsKenmerk(boekRegel.getSpecificatie().getBetaalopdrachtRegel().getBetaalopdracht().getBetalingskenmerk());
		}
		betalingDto.setScreenitId(boekRegel.getId());
		betalingDto.setDebet(boekRegel.getDebet());
		betalingDto.setBedrag(huisartsBedrag);

		return betalingDto;
	}

	private static AdresDto getAdresDto(CervixHuisartsAdres adres)
	{
		AdresDto adresDto = new AdresDto();
		adresDto.setScreenitId(adres.getScreenitId());
		adresDto.setHuisartsportaalId(adres.getHuisartsportaalId());
		adresDto.setStraat(adres.getStraat());
		adresDto.setHuisnummer(adres.getHuisnummer());
		adresDto.setPostcode(adres.getPostcode());
		adresDto.setWoonplaats(getWoonplaatsDto(adres.getWoonplaats()));
		adresDto.setHuisnummertoevoeging(adres.getHuisnummerToevoeging());
		return adresDto;
	}

	private static WoonplaatsDto getWoonplaatsDto(Woonplaats woonplaats)
	{
		WoonplaatsDto woonplaatsDto = new WoonplaatsDto();
		woonplaatsDto.setScreenitId(woonplaats.getId());
		woonplaatsDto.setNaam(woonplaats.getNaam());
		woonplaatsDto.setCode(woonplaats.getCode());
		woonplaatsDto.setGemeente(woonplaats.getGemeente().getNaam());
		return woonplaatsDto;
	}

}
