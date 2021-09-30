package nl.rivm.screenit.huisartsenportaal.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
 * %%
 * Copyright (C) 2016 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.io.Serializable;

import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.Session;

import nl.rivm.screenit.huisartsenportaal.dto.AanvraagDto;
import nl.rivm.screenit.huisartsenportaal.dto.AdresDto;
import nl.rivm.screenit.huisartsenportaal.dto.HuisartsDto;
import nl.rivm.screenit.huisartsenportaal.dto.LocatieDto;
import nl.rivm.screenit.huisartsenportaal.dto.WoonplaatsDto;
import nl.rivm.screenit.huisartsenportaal.model.Adres;
import nl.rivm.screenit.huisartsenportaal.model.Huisarts;
import nl.rivm.screenit.huisartsenportaal.model.LabformulierAanvraag;
import nl.rivm.screenit.huisartsenportaal.model.Locatie;
import nl.rivm.screenit.huisartsenportaal.model.Woonplaats;
import nl.rivm.screenit.huisartsenportaal.service.SynchronisatieService;

import org.apache.activemq.command.ActiveMQObjectMessage;
import org.modelmapper.ModelMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.jms.core.JmsTemplate;
import org.springframework.jms.core.MessageCreator;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.ObjectWriter;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class SynchronisatieServiceImpl implements SynchronisatieService
{

	private static final Logger LOG = LoggerFactory.getLogger(SynchronisatieServiceImpl.class);

	@Autowired
	private JmsTemplate jmsTemplate;

	@Autowired
	private ModelMapper modelMapper;

	@Value("${nl.rivm.huisartsportaal.activemq.listener}")
	private String destination;

	private static final ObjectMapper objectMapper = new ObjectMapper();

	@Override
	public void syncHuisarts(Huisarts huisarts)
	{
		HuisartsDto dto = getHuisartsDto(huisarts);
		sendingObject(destination, dto);
	}

	@Override
	public void syncAanvraag(LabformulierAanvraag aanvraag)
	{
		AanvraagDto dto = new AanvraagDto();
		modelMapper.map(aanvraag, dto);
		sendingObject(destination, dto);
	}

	@Override
	public void syncLocatie(Huisarts huisarts, Locatie locatie, Boolean herzendVerificatieMail)
	{
		LocatieDto dto = getLocatieDto(huisarts, locatie, herzendVerificatieMail);
		sendingObject(destination, dto);
	}

	private void sendingObject(String destination, Serializable object)
	{
		try
		{
			ObjectWriter writer = objectMapper.writer();
			LOG.info(object.getClass().getSimpleName() + ": " + writer.writeValueAsString(object));
		}
		catch (JsonProcessingException e)
		{
			LOG.error("Fout bij maken JSON voor ActiveMQObjectMessage bericht", e);
		}
		jmsTemplate.send(destination, new MessageCreator()
		{
			@Override
			public Message createMessage(Session session) throws JMSException
			{
				ActiveMQObjectMessage messageObject = new ActiveMQObjectMessage();

				messageObject.setObject(object);

				return messageObject;
			}
		});
	}

	@Override
	public HuisartsDto getHuisartsDto(Huisarts huisarts)
	{
		HuisartsDto dto = new HuisartsDto();
		dto.setScreenitId(huisarts.getScreenitId());
		dto.setHuisartsportaalId(huisarts.getHuisartsportaalId());
		dto.setAgbcode(huisarts.getAgbcode());
		dto.setUsername(huisarts.getGebruikersnaam());
		dto.setEmail(huisarts.getEmail());
		dto.setAanhef(huisarts.getAanhef());
		dto.setAchternaam(huisarts.getAchternaam());
		dto.setTussenvoegsel(huisarts.getTussenvoegsel());
		dto.setVoorletters(huisarts.getVoorletters());
		dto.setTelefoon(huisarts.getTelefoon());
		dto.setInlogCode(huisarts.getInlogCode());
		dto.setActief(huisarts.getActief());
		dto.setAanmeldStatus(huisarts.getAanmeldStatus().name());
		dto.setExtraEmails(huisarts.getExtraEmails());
		if (huisarts.getPostadres() != null)
		{
			dto.setPostadres(getAdresDto(huisarts.getPostadres()));
		}
		return dto;
	}

	@Override
	public void herzendVerificatieMail(Huisarts huisarts, Locatie locatie)
	{
		LocatieDto dto = getLocatieDto(huisarts, locatie, true);
		sendingObject(destination, dto);
	}

	private LocatieDto getLocatieDto(Huisarts huisarts, Locatie locatie, Boolean herzendVerificatieMail)
	{
		LocatieDto locatieDto = new LocatieDto();
		locatieDto.setScreenitId(locatie.getScreenitId());
		locatieDto.setHuisartsportaalId(locatie.getHuisartsportaalId());
		locatieDto.setIban(locatie.getIban());
		locatieDto.setIbanTenaamstelling(locatie.getIbanTenaamstelling());
		locatieDto.setNaam(locatie.getNaam());
		locatieDto.setZorgmailklantnummer(locatie.getZorgmailklantnummer());
		locatieDto.setHuisartsId(huisarts.getScreenitId());
		locatieDto.setStatus(locatie.getStatus().name());
		locatieDto.setVerificatieCode(locatie.getVerificatieCode());
		locatieDto.setHerzendVerificatieMail(herzendVerificatieMail);
		locatieDto.setMoetVerifierenVoorActivatie(locatie.getMoetVerifierenVoorActivatie());
		if (locatie.getLocatieAdres() != null)
		{
			locatieDto.setLocatieAdres(getAdresDto(locatie.getLocatieAdres()));
		}
		return locatieDto;
	}

	private AdresDto getAdresDto(Adres adres)
	{
		AdresDto adresDto = new AdresDto();
		adresDto.setHuisartsportaalId(adres.getHuisartsportaalId());
		adresDto.setScreenitId(adres.getScreenitId());
		adresDto.setStraat(adres.getStraat());
		adresDto.setHuisnummer(adres.getHuisnummer());
		adresDto.setHuisnummertoevoeging(adres.getHuisnummertoevoeging());
		adresDto.setPostcode(adres.getPostcode());
		adresDto.setWoonplaats(getWoonplaatsDto(adres.getWoonplaats()));
		return adresDto;
	}

	private WoonplaatsDto getWoonplaatsDto(Woonplaats woonplaats)
	{
		WoonplaatsDto woonplaatsDto = new WoonplaatsDto();
		woonplaatsDto.setHuisartsportaalId(woonplaats.getHuisartsportaalId());
		woonplaatsDto.setScreenitId(woonplaats.getScreenitId());
		woonplaatsDto.setNaam(woonplaats.getNaam());
		woonplaatsDto.setGemeente(woonplaats.getGemeente());
		woonplaatsDto.setCode(woonplaats.getCode());
		return woonplaatsDto;
	}
}
