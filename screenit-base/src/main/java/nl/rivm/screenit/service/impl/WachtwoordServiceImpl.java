package nl.rivm.screenit.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.Date;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import lombok.NonNull;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.service.WachtwoordService;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.EntityAuditUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.shiro.crypto.hash.Sha512Hash;
import org.hibernate.envers.query.AuditEntity;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.Assert;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class WachtwoordServiceImpl implements WachtwoordService
{

	@Autowired
	private HibernateService hibernateService;

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
		medewerker.setLaatsteKeerWachtwoordGewijzigd(new Date());
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

}
