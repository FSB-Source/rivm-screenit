package nl.rivm.screenit.service.impl;

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

import java.time.LocalDate;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import lombok.NonNull;
import lombok.RequiredArgsConstructor;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.MailService;
import nl.rivm.screenit.service.WachtwoordService;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.EntityAuditUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.shiro.crypto.hash.Sha512Hash;
import org.hibernate.envers.query.AuditEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.Assert;

@Service
@RequiredArgsConstructor
public class WachtwoordServiceImpl implements WachtwoordService
{

	private final HibernateService hibernateService;

	private final ICurrentDateSupplier currentDateSupplier;

	private final MailService mailService;

	private final LogService logService;

	@Override
	public String hashWachtwoord(Gebruiker gebruiker, String plainWachtwoord)
	{
		Sha512Hash hash = new Sha512Hash(plainWachtwoord, gebruiker.getId().toString(), Constants.PASSWORDHASHINGITERATIONS);
		return hash.toHex();
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void setWachtwoord(@NonNull Gebruiker medewerker, String uncryptedWachtwoord)
	{
		Assert.isTrue(medewerker.getId() != null, "Medewerker moet opgeslagen zijn.");
		medewerker.setWachtwoord(hashWachtwoord(medewerker, uncryptedWachtwoord));
		medewerker.setLaatsteKeerWachtwoordGewijzigd(currentDateSupplier.getDate());
		medewerker.setWachtwoordVerlooptWaarschuwingVerzonden(false);
	}

	@Override
	public boolean isEerderGebruiktWachtwoord(@NonNull Gebruiker medewerker, String unecryptedWachtwoord, List<String> vorigeWachtwoorden)
	{
		String hashedWachtwoord = hashWachtwoord(medewerker, unecryptedWachtwoord);

		return vorigeWachtwoorden
			.stream()
			.filter(Objects::nonNull)
			.anyMatch(e -> e.equals(hashedWachtwoord));
	}

	@Override
	public List<String> getVorigeWachtwoorden(Gebruiker gebruiker, LocalDate vanaf)
	{
		return EntityAuditUtil.getEntityHistory(gebruiker, hibernateService.getHibernateSession(),
				AuditEntity.revisionProperty("timestamp").gt(DateUtil.toUtilDate(vanaf).getTime()), true)
			.stream()
			.map((Object auditRow) ->
			{
				Gebruiker gebruikerAtRevision = EntityAuditUtil.getRevisionEntity(auditRow);
				return gebruikerAtRevision.getWachtwoord();
			}).collect(Collectors.toList());
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void verstuurWachtwoordVerlooptHerinneringMail(Gebruiker gebruiker)
	{
		mailService.sendWachwoordVerlooptHerinneringMail(gebruiker);
		gebruiker.setWachtwoordVerlooptWaarschuwingVerzonden(true);
		hibernateService.saveOrUpdate(gebruiker);
		logService.logGebeurtenis(LogGebeurtenis.WACHTWOORD_VERLOOPT_HERINNERING_VERSTUURD, gebruiker);
	}

}
