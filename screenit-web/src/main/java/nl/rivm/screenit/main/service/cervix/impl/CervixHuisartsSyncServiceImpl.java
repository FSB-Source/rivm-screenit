package nl.rivm.screenit.main.service.cervix.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.Date;
import java.util.Optional;

import nl.rivm.screenit.huisartsenportaal.dto.AanvraagDto;
import nl.rivm.screenit.huisartsenportaal.dto.AdresDto;
import nl.rivm.screenit.huisartsenportaal.dto.HuisartsDto;
import nl.rivm.screenit.huisartsenportaal.dto.LocatieDto;
import nl.rivm.screenit.huisartsenportaal.dto.OvereenkomstDto;
import nl.rivm.screenit.huisartsenportaal.dto.ResetDto;
import nl.rivm.screenit.huisartsenportaal.dto.WoonplaatsDto;
import nl.rivm.screenit.huisartsenportaal.enums.CervixLocatieStatus;
import nl.rivm.screenit.main.service.cervix.CervixHuisartsSyncService;
import nl.rivm.screenit.model.Aanhef;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.Woonplaats;
import nl.rivm.screenit.model.cervix.CervixHuisarts;
import nl.rivm.screenit.model.cervix.CervixHuisartsAdres;
import nl.rivm.screenit.model.cervix.CervixHuisartsLocatie;
import nl.rivm.screenit.model.cervix.CervixLabformulierAanvraag;
import nl.rivm.screenit.model.cervix.enums.CervixHuisartsAanmeldStatus;
import nl.rivm.screenit.model.cervix.enums.CervixHuisartsLocatieMutatieSoort;
import nl.rivm.screenit.model.cervix.enums.CervixLabformulierAanvraagStatus;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.overeenkomsten.Overeenkomst;
import nl.rivm.screenit.repository.algemeen.WoonplaatsRepository;
import nl.rivm.screenit.repository.cervix.CervixHuisartsAdresRepository;
import nl.rivm.screenit.repository.cervix.CervixHuisartsLocatieRepository;
import nl.rivm.screenit.repository.cervix.CervixHuisartsRepository;
import nl.rivm.screenit.repository.cervix.CervixLabformulierAanvraagRepository;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.HuisartsenportaalSyncService;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.MailService;
import nl.rivm.screenit.service.cervix.CervixHuisartsBerichtService;
import nl.rivm.screenit.service.impl.DefaultCurrentDateSupplier;
import nl.rivm.screenit.util.NaamUtil;
import nl.rivm.screenit.util.cervix.CervixHuisartsToDtoUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class CervixHuisartsSyncServiceImpl implements CervixHuisartsSyncService
{

	@Autowired
	private MailService mailService;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private DefaultCurrentDateSupplier currentDateSupplier;

	@Autowired
	private BaseBriefService briefService;

	@Autowired
	private LogService logService;

	@Autowired
	private CervixHuisartsBerichtService huisartsBerichtService;

	@Autowired
	private HuisartsenportaalSyncService huisartsenportaalSyncService;

	@Autowired
	private CervixHuisartsAdresRepository huisartsAdresRepository;

	@Autowired
	private CervixLabformulierAanvraagRepository labformulierAanvraagRepository;

	@Autowired
	private CervixHuisartsLocatieRepository huisartsLocatieRepository;

	@Autowired
	private CervixHuisartsRepository huisartsRepository;

	@Autowired
	private WoonplaatsRepository woonplaatsRepository;

	@Override
	public void sendData(CervixHuisarts cervixHuisarts)
	{
		var huisartsDto = CervixHuisartsToDtoUtil.getHuisartsDto(cervixHuisarts);

		huisartsenportaalSyncService.sendJmsBericht(huisartsDto);
	}

	@Override
	public void sendData(ResetDto resetDto)
	{
		huisartsenportaalSyncService.sendJmsBericht(resetDto);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public CervixHuisarts updateAndGetHuisarts(HuisartsDto dto, Date mutatieDatum)
	{
		var huisarts = Optional.ofNullable(dto.getScreenitId())
			.flatMap(id -> huisartsRepository.findById(id))
			.or(() -> Optional.ofNullable(dto.getHuisartsportaalId())
				.flatMap(id -> huisartsRepository.findOneByHuisartsportaalId(id)))
			.orElseThrow(() -> new IllegalStateException("Onbekende huisarts voor ScreenIT, synchronisatie gestopt."));

		huisarts.setScreenitId(dto.getScreenitId());
		huisarts.setHuisartsportaalId(dto.getHuisartsportaalId());
		huisarts.setAgbcode(dto.getAgbcode());
		huisarts.setGebruikersnaamHuisartsenPortaal(dto.getUsername());
		huisarts.setEmail(dto.getEmail());
		huisarts.setMutatiedatum(mutatieDatum);
		Gebruiker gebruiker = huisarts.getOrganisatieMedewerkers().get(0).getMedewerker();
		gebruiker.setAchternaam(dto.getAchternaam());
		gebruiker.setTussenvoegsel(dto.getTussenvoegsel());
		gebruiker.setVoorletters(dto.getVoorletters());
		gebruiker.setAanhef(Aanhef.getAanhefWithName(dto.getAanhef()));
		huisarts.setNaam(getPraktijkNaam(gebruiker));
		huisarts.setTelefoon(dto.getTelefoon());
		huisarts.setActief(dto.getActief());
		huisarts.setAanmeldStatus(CervixHuisartsAanmeldStatus.valueOf(dto.getAanmeldStatus()));
		huisarts.setExtraEmails(dto.getExtraEmails());
		if (dto.getInlogCode() != null)
		{
			setWachtwoordInlogcode(huisarts, dto.getInlogCode());
		}
		else
		{
			gebruiker.setWachtwoordChangeCode(null);
		}
		if (dto.getPostadres() != null)
		{
			huisarts.setPostadres(setAdres(dto.getPostadres(), mutatieDatum));
		}
		huisartsRepository.save(huisarts);
		logService.logGebeurtenis(LogGebeurtenis.ORGANISATIE_WIJZIG, huisarts.getOrganisatieMedewerkers().get(0), "Huisarts: " + huisarts.getNaam(), Bevolkingsonderzoek.CERVIX);
		return huisarts;
	}

	@Override
	public String getPraktijkNaam(Gebruiker gebruiker)
	{
		var builder = new StringBuilder();
		builder.append("Praktijk van ");
		builder.append(NaamUtil.getTussenvoegselEnAchternaam(gebruiker));
		return builder.toString();
	}

	private CervixHuisarts setWachtwoordInlogcode(CervixHuisarts huisarts, String inlogCode)
	{
		var medewerker = huisarts.getOrganisatieMedewerkers().get(0).getMedewerker();
		medewerker.setWachtwoordChangeCode(inlogCode);
		if (CervixHuisartsAanmeldStatus.GEREGISTREERD.equals(huisarts.getAanmeldStatus()) && StringUtils.isNotBlank(inlogCode))
		{
			mailService.sendPasswordResetMail(huisarts);
		}
		return huisarts;
	}

	private CervixHuisartsLocatie setLocatie(CervixHuisarts huisarts, LocatieDto dto, Date mutatieDatum)
	{
		var locatie = Optional.ofNullable(dto.getScreenitId())
			.flatMap(id -> huisartsLocatieRepository.findById(id))
			.or(() -> Optional.ofNullable(dto.getHuisartsportaalId())
				.flatMap(id -> huisartsLocatieRepository.findOneByHuisartsportaalId(id)))
			.orElse(new CervixHuisartsLocatie());

		locatie.setHuisartsportaalId(dto.getHuisartsportaalId());
		locatie.setScreenitId(dto.getScreenitId());
		locatie.setMutatiedatum(mutatieDatum);
		locatie.setMutatieSoort(getMutatieSoort(locatie, dto));
		locatie.setIban(dto.getIban());
		locatie.setIbanTenaamstelling(dto.getIbanTenaamstelling());
		locatie.setNaam(dto.getNaam());
		locatie.setVerificatieCode(dto.getVerificatieCode());
		locatie.setMoetVerifierenVoorActivatie(dto.getMoetVerifierenVoorActivatie());

		if (!CervixLocatieStatus.INACTIEF.equals(locatie.getStatus()) && CervixLocatieStatus.INACTIEF.name().equals(dto.getStatus()))
		{
			logService.logGebeurtenis(LogGebeurtenis.CERVIX_HUISARTSLOCATIE_VERWIJDERD, huisarts.getOrganisatieMedewerkers().get(0),
				"Huisarts: " + huisarts.getNaam() + " locatie: " + locatie.getNaam(), Bevolkingsonderzoek.CERVIX);
		}
		locatie.setStatus(CervixLocatieStatus.valueOf(dto.getStatus()));

		locatie.setZorgmailklantnummer(dto.getZorgmailklantnummer());
		locatie.setHuisarts(huisarts);

		if (dto.getLocatieAdres() != null)
		{
			locatie.setLocatieAdres(setAdres(dto.getLocatieAdres(), mutatieDatum));
		}
		huisartsLocatieRepository.save(locatie);
		logService.logGebeurtenis(LogGebeurtenis.ORGANISATIE_WIJZIG, huisarts.getOrganisatieMedewerkers().get(0),
			"Huisarts: " + huisarts.getNaam() + " locatie: " + locatie.getNaam(), Bevolkingsonderzoek.CERVIX);
		return locatie;
	}

	@Override
	public CervixHuisartsLocatieMutatieSoort getMutatieSoort(CervixHuisartsLocatie locatie, LocatieDto locatieDto)
	{
		if (locatie == null || locatie.getId() == null
			|| (!CervixLocatieStatus.INACTIEF.equals(CervixLocatieStatus.valueOf(locatieDto.getStatus())) && CervixLocatieStatus.INACTIEF.equals(locatie.getStatus())))
		{
			return CervixHuisartsLocatieMutatieSoort.NIEUW;
		}

		if (CervixLocatieStatus.INACTIEF.name().equals(locatieDto.getStatus()) && !CervixLocatieStatus.INACTIEF.equals(locatie.getStatus()))
		{
			return CervixHuisartsLocatieMutatieSoort.GEINACTIVEERD;
		}

		var adresDto = locatieDto.getLocatieAdres();
		var adres = locatie.getLocatieAdres();
		if (!StringUtils.equals(adres.getStraat(), adresDto.getStraat()) ||
			adres.getHuisnummer().intValue() != adresDto.getHuisnummer().intValue() ||
			!StringUtils.equals(adres.getHuisnummerToevoeging(), adresDto.getHuisnummertoevoeging()) ||
			!StringUtils.equals(adres.getPostcode(), adresDto.getPostcode()) ||
			!StringUtils.equals(adres.getWoonplaats().getNaam(), adresDto.getWoonplaats().getNaam()) ||
			!StringUtils.equals(adres.getWoonplaats().getGemeente().getNaam(), adresDto.getWoonplaats().getGemeente()))
		{
			return CervixHuisartsLocatieMutatieSoort.ADRES_WIJZIGING;
		}
		return CervixHuisartsLocatieMutatieSoort.OVERIGE;
	}

	private CervixHuisartsAdres setAdres(AdresDto dto, Date mutatieDatum)
	{
		var adres = Optional.ofNullable(dto.getScreenitId())
			.flatMap(id -> huisartsAdresRepository.findById(id))
			.or(() -> Optional.ofNullable(dto.getHuisartsportaalId())
				.flatMap(id -> huisartsAdresRepository.findOneByHuisartsportaalId(id)))
			.orElse(new CervixHuisartsAdres());

		adres.setScreenitId(dto.getScreenitId());
		adres.setHuisartsportaalId(dto.getHuisartsportaalId());
		adres.setMutatiedatum(mutatieDatum);
		adres.setStraat(dto.getStraat());
		adres.setHuisnummer(dto.getHuisnummer());
		adres.setHuisnummerToevoeging(dto.getHuisnummertoevoeging());
		adres.setWoonplaats(setWoonplaats(dto.getWoonplaats()));
		adres.setPostcode(dto.getPostcode());
		adres.setGbaGemeente(adres.getWoonplaats().getGemeente());
		huisartsAdresRepository.save(adres);
		return adres;
	}

	private Woonplaats setWoonplaats(WoonplaatsDto dto)
	{
		return woonplaatsRepository.findById(dto.getScreenitId()).orElse(null);
	}

	@Override
	public void sendData(Overeenkomst overeenkomst)
	{
		var overeenkomstDto = new OvereenkomstDto();
		overeenkomstDto.setPath(overeenkomst.getDocument().getPath());
		overeenkomstDto.setNaam(overeenkomst.getDocument().getNaam());
		overeenkomstDto.setScreenitId(overeenkomst.getId());
		overeenkomstDto.setLaatsteWijzigDatum(overeenkomst.getLaatsteUpdateDocument());

		huisartsenportaalSyncService.sendJmsBericht(overeenkomstDto);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void updateHuisarts(HuisartsDto huisartsDto)
	{
		var mutatieDatum = currentDateSupplier.getDate();
		CervixHuisarts arts = updateAndGetHuisarts(huisartsDto, mutatieDatum);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void nieuweAanvraagLabformulieren(AanvraagDto aanvraagDto)
	{
		var nu = currentDateSupplier.getDate();

		var locatie = Optional.ofNullable(aanvraagDto.getLocatie().getScreenitId())
			.flatMap(id -> huisartsLocatieRepository.findById(id))
			.or(() -> Optional.ofNullable(aanvraagDto.getLocatie().getHuisartsportaalId())
				.flatMap(id -> huisartsLocatieRepository.findOneByHuisartsportaalId(id)))
			.orElseThrow(() -> new IllegalStateException("Locatie niet gevonden in de database. Kan niet worden gescynchroniseerd!"));

		var huisarts = locatie.getHuisarts();
		var aanvraag = new CervixLabformulierAanvraag();
		aanvraag.setAanvraagDatum(aanvraagDto.getAanvraagDatum());
		aanvraag.setHuisartsportaalId(aanvraagDto.getHuisartsportaalId());
		aanvraag.setMutatiedatum(nu);
		aanvraag.setAantal(aanvraagDto.getAantal());
		aanvraag.setInstellingGebruiker(huisarts.getOrganisatieMedewerkers().get(0));
		aanvraag.setStatus(CervixLabformulierAanvraagStatus.valueOf(aanvraagDto.getStatus()));
		aanvraag.setStatusDatum(aanvraagDto.getStatusDatum());
		aanvraag.setHuisartsLocatie(locatie);

		var so = locatie.getLocatieAdres().getGbaGemeente().getScreeningOrganisatie();
		var labformulierenBrief = briefService.maakRegioBrief(so, BriefType.REGIO_UITSTRIJKEND_ARTS_LABFORMULIER, nu, null);
		aanvraag.setBrief(labformulierenBrief);

		var voorbladBrief = briefService.maakRegioBrief(so, BriefType.REGIO_UITSTRIJKEND_ARTS_VOORBLAD_LABFORMULIER, nu, null);
		aanvraag.setVoorbladBrief(voorbladBrief);

		labformulierAanvraagRepository.save(aanvraag);
	}

	@Override
	public void sendData(CervixLabformulierAanvraag labformulierAanvraag)
	{
		var dto = CervixHuisartsToDtoUtil.getAanvraagDto(labformulierAanvraag);
		huisartsenportaalSyncService.sendJmsBericht(dto);
	}

	@Override
	public void sendData(CervixHuisartsLocatie locatie)
	{
		var dto = CervixHuisartsToDtoUtil.getLocatieDto(locatie);
		huisartsenportaalSyncService.sendJmsBericht(dto);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void updateLocatie(LocatieDto locatieDto)
	{
		var mutatieDatum = currentDateSupplier.getDate();
		var huisarts = hibernateService.load(CervixHuisarts.class, locatieDto.getHuisartsId());
		var locatie = setLocatie(huisarts, locatieDto, mutatieDatum);
		if (CervixLocatieStatus.KLANTNUMMER_NIET_GEVERIFIEERD.equals(locatie.getStatus()) && !Boolean.FALSE.equals(locatieDto.getHerzendVerificatieMail()))
		{
			huisartsBerichtService.sendKlantnummerVerificatieMail(huisarts, locatie);
		}
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void setLabformulierAanvraag(AanvraagDto aanvraagDto)
	{
		var aanvraag = Optional.ofNullable(aanvraagDto.getScreenitId())
			.flatMap(id -> labformulierAanvraagRepository.findById(id))
			.or(() -> Optional.ofNullable(aanvraagDto.getHuisartsportaalId())
				.flatMap(id -> labformulierAanvraagRepository.findOneByHuisartsportaalId(id)))
			.map(a ->
			{
				a.setAanvraagDatum(aanvraagDto.getAanvraagDatum());
				a.setMutatiedatum(currentDateSupplier.getDate());
				a.setAantal(aanvraagDto.getAantal());
				a.setStatus(CervixLabformulierAanvraagStatus.valueOf(aanvraagDto.getStatus()));
				a.setStatusDatum(aanvraagDto.getStatusDatum());
				return a;
			});

		if (aanvraag.isEmpty())
		{
			nieuweAanvraagLabformulieren(aanvraagDto);
		}
		else
		{
			labformulierAanvraagRepository.save(aanvraag.get());
		}
	}
}
