package nl.rivm.screenit.huisartsenportaal.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
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

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.huisartsenportaal.dto.HuisartsDto;
import nl.rivm.screenit.huisartsenportaal.dto.WachtwoordVergetenDto;
import nl.rivm.screenit.huisartsenportaal.model.Huisarts;
import nl.rivm.screenit.huisartsenportaal.model.Medewerker;
import nl.rivm.screenit.huisartsenportaal.model.enums.AanmeldStatus;
import nl.rivm.screenit.huisartsenportaal.model.enums.InlogMethode;
import nl.rivm.screenit.huisartsenportaal.model.enums.Recht;
import nl.rivm.screenit.huisartsenportaal.repository.HuisartsRepository;
import nl.rivm.screenit.huisartsenportaal.service.AdresService;
import nl.rivm.screenit.huisartsenportaal.service.HuisartsService;
import nl.rivm.screenit.huisartsenportaal.service.LocatieService;
import nl.rivm.screenit.huisartsenportaal.util.CodeGenerator;
import nl.rivm.screenit.huisartsenportaal.util.DateUtil;

import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class HuisartsServiceImpl implements HuisartsService
{

	@Autowired
	private AdresService adresService;

	@Autowired
	private HuisartsRepository huisartsRepository;

	@Autowired
	private LocatieService locatieService;

	@Autowired
	private PasswordEncoder passwordEncoder;

	@Autowired
	private ModelMapper modelMapper;

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public Huisarts updateAndGetHuisarts(HuisartsDto huisartsDto, Huisarts huisarts)
	{
		modelMapper.map(huisartsDto, huisarts);

		if (huisartsDto.getUsername() != null && huisartsDto.getWachtwoord() != null && !InlogMethode.USERNAME_PASSWORD.equals(huisarts.getInlogMethode()))
		{
			huisarts.setGebruikersnaam(huisartsDto.getUsername());

			String wachtwoord = huisartsDto.getWachtwoord();
			updateWachtwoord(huisarts, wachtwoord);

			huisarts.getRollen().remove(Recht.ROLE_REGISTEREN);
			huisarts.getRollen().add(Recht.ROLE_AANVRAGEN);
			huisarts.setActief(true);
		}
		if (huisartsDto.getOvereenkomst())
		{
			huisarts.setOvereenkomstGeaccordeerdDatum(new Date());
		}

		adresService.updateAndGetAdres(huisartsDto.getPostadres());

		huisartsRepository.save(huisarts);
		return huisarts;
	}

	@Override
	public Huisarts updateWachtwoord(Huisarts huisarts, String wachtwoord)
	{
		String encodedWachtwoord = passwordEncoder.encode(wachtwoord);
		huisarts.setPassword(encodedWachtwoord);
		huisarts.setInlogMethode(InlogMethode.USERNAME_PASSWORD);
		huisarts.setAanmeldStatus(AanmeldStatus.GEREGISTREERD);
		huisarts.getRollen().remove(Recht.ROLE_REGISTEREN);
		if (!huisarts.getRollen().contains(Recht.ROLE_AANVRAGEN))
		{
			huisarts.getRollen().add(Recht.ROLE_AANVRAGEN);
		}
		huisarts.setInlogCode(null);
		return huisarts;
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public Huisarts updateAndGetHuisarts(HuisartsDto huisartsDto)
	{
		Huisarts huisarts = null;
		if (huisartsDto.getHuisartsportaalId() != null)
		{
			huisarts = huisartsRepository.findByHuisartsportaalId(huisartsDto.getHuisartsportaalId());
		}
		if (huisarts == null && huisartsDto.getScreenitId() != null)
		{
			huisarts = huisartsRepository.findByScreenitId(huisartsDto.getScreenitId());
		}
		if (huisarts == null)
		{
			huisarts = new Huisarts();
		}
		if (huisarts.getScreenitId() == null)
		{
			huisarts.setScreenitId(huisartsDto.getScreenitId());
		}
		huisarts.setAgbcode(huisartsDto.getAgbcode());
		huisarts.setEmail(huisartsDto.getEmail());
		huisarts.setAanhef(huisartsDto.getAanhef());
		huisarts.setAchternaam(huisartsDto.getAchternaam());
		huisarts.setTussenvoegsel(huisartsDto.getTussenvoegsel());
		huisarts.setVoorletters(huisartsDto.getVoorletters());
		huisarts.setTelefoon(huisartsDto.getTelefoon());
		huisarts.setActief(huisartsDto.getActief());
		huisarts.setExtraEmails(huisartsDto.getExtraEmails());
		huisarts.setAanmeldStatus(AanmeldStatus.valueOf(huisartsDto.getAanmeldStatus()));
		if (huisartsDto.getInlogCode() != null && AanmeldStatus.GEREGISTREERD != huisarts.getAanmeldStatus())
		{
			huisarts.setAttempts(0);
			huisarts.setInlogCode(huisartsDto.getInlogCode());
			huisarts.setInlogMethode(InlogMethode.INLOGCODE);
		}
		huisarts.setInlogCode(huisartsDto.getInlogCode());
		if (huisartsDto.getPostadres() != null)
		{
			huisarts.setPostadres(adresService.updateAndGetAdres(huisartsDto.getPostadres()));
		}

		huisartsRepository.save(huisarts);
		return huisarts;
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public Huisarts wachtwoordVergeten(Huisarts huisarts) throws IllegalStateException
	{
		if (huisarts != null && InlogMethode.USERNAME_PASSWORD.equals(huisarts.getInlogMethode()))
		{
			String codeB = CodeGenerator.genereerCode(3, 3);
			huisarts.setInlogCode(codeB);
			List<Recht> rechten = new ArrayList<>();
			rechten.add(Recht.ROLE_REGISTEREN);
			huisarts.setRollen(rechten);
			huisarts.setAttempts(0);
			huisartsRepository.save(huisarts);
			return huisarts;
		}
		else if (huisarts != null && InlogMethode.INLOGCODE.equals(huisarts.getInlogMethode()))
		{
			throw new IllegalStateException("U zult zich eerst moeten registreren voordat u een wachtwoord kan aanvragen");
		}
		throw new IllegalStateException("Er is geen huisarts gevonden met deze gebruikersnaam en e-mail");
	}

	@Override
	public Huisarts getHuisartsWith(WachtwoordVergetenDto dto)
	{
		return huisartsRepository.findByEmailAndGebruikersnaam(dto.getEmail(), dto.getGebruikersnaam());
	}

	@Override
	public boolean controleerWachtwoord(String plainWachtwoord, String encodedWachtwoord)
	{
		return passwordEncoder.matches(plainWachtwoord, encodedWachtwoord);
	}

	public void setPasswordEncoder(PasswordEncoder passwordEncoder)
	{
		this.passwordEncoder = passwordEncoder;
	}

	@Override
	public Huisarts getHuisartsWith(Long screenitId)
	{
		return huisartsRepository.findByScreenitId(screenitId);
	}

	@Override
	public Integer incrementAttempts(Huisarts huisarts)
	{
		Integer attempts = huisarts.getAttempts();
		if (attempts == Medewerker.MAX_ATTEMPS)
		{
			attempts = 0;
		}
		huisarts.setAttempts(++attempts);
		huisarts.setLastAttemptDate(new Date());
		huisartsRepository.save(huisarts);
		return Medewerker.MAX_ATTEMPS - attempts + 1; 
	}

	@Override
	public void resetAttempts(Huisarts huisarts)
	{
		huisarts.setAttempts(0);
		huisarts.setLastAttemptDate(new Date());
		huisartsRepository.save(huisarts);
	}

	@Override
	public Long remainingMinutesLock(Huisarts huisarts)
	{
		Date lastAttempt = huisarts.getLastAttemptDate();
		Date lockdownTime = DateUtil.toUtilDate(LocalDateTime.now().minusMinutes(Medewerker.MAX_LOCKED));
		long diffMs = lastAttempt.getTime() - lockdownTime.getTime();
		long diffsec = diffMs / 1000;
		long minuten = diffsec / 60 + 1;
		return minuten > 0 ? minuten : 0;
	}
}
