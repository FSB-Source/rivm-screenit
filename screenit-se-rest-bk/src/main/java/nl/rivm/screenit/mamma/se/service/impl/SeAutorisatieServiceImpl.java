package nl.rivm.screenit.mamma.se.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.mamma.se.dto.SERechtDto;
import nl.rivm.screenit.mamma.se.dto.SeAutorisatieDto;
import nl.rivm.screenit.mamma.se.security.SERealm;
import nl.rivm.screenit.mamma.se.service.SeAutorisatieService;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.InstellingGebruikerRol;
import nl.rivm.screenit.model.Permissie;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.security.Constraint;
import nl.rivm.screenit.security.ScreenitPrincipal;
import nl.rivm.screenit.service.AutorisatieService;

import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import org.apache.commons.lang.ArrayUtils;
import org.apache.shiro.mgt.SecurityManager;
import org.apache.shiro.subject.PrincipalCollection;
import org.apache.shiro.subject.SimplePrincipalCollection;
import org.apache.shiro.util.ThreadContext;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.time.LocalDate;

@Service
public class SeAutorisatieServiceImpl implements SeAutorisatieService
{

	@Autowired
	private AutorisatieService autorisatieService;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private SERealm seRealm;

	@Override
	public boolean isGeautoriseerd(Long accountId, Recht recht)
	{
		return checkPermission(accountId, recht, getBenodigdeActie(recht));
	}

	@Override
	public boolean isGeautoriseerdVoorInloggen(Long accountId)
	{
		return isGeautoriseerd(accountId, Recht.GEBRUIKER_SCREENING_MAMMA_SE_INSCHRIJVEN)
			|| isGeautoriseerd(accountId, Recht.GEBRUIKER_SCREENING_MAMMA_SE_CONNECTIESTATUS_INZIEN);
	}

	@Override
	public boolean isInstellingGebruikerGeautoriseerd(InstellingGebruiker instellingGebruiker, Recht recht)
	{

		return autorisatieService.getActieVoorMedewerker(instellingGebruiker, null, recht) == getBenodigdeActie(recht);
	}

	@Override
	public SeAutorisatieDto getSeRechten(Long accountId)
	{
		SeAutorisatieDto dto = new SeAutorisatieDto();
		InstellingGebruiker instellingGebruiker = hibernateService.get(InstellingGebruiker.class, accountId);
		PrincipalCollection principals = createSimplePrincipalCollection(accountId);
		SecurityManager securityManager = ThreadContext.getSecurityManager();

		dto.setInschrijvenRecht(getSeRechtDto(instellingGebruiker, securityManager, principals, Recht.GEBRUIKER_SCREENING_MAMMA_SE_INSCHRIJVEN));
		dto.setOnderzoekenRecht(getSeRechtDto(instellingGebruiker, securityManager, principals, Recht.GEBRUIKER_SCREENING_MAMMA_SE_ONDERZOEK));
		dto.setSignalerenRecht(getSeRechtDto(instellingGebruiker, securityManager, principals, Recht.GEBRUIKER_SCREENING_MAMMA_SE_SIGNALEREN));
		dto.setKwaliteitsopnameRecht(getSeRechtDto(instellingGebruiker, securityManager, principals, Recht.GEBRUIKER_SCREENING_MAMMA_SE_KWALITEITSOPNAME));
		dto.setConnectiestatusRecht(getSeRechtDto(instellingGebruiker, securityManager, principals, Recht.GEBRUIKER_SCREENING_MAMMA_SE_CONNECTIESTATUS_INZIEN));

		dto.setInschrijven(dto.getInschrijvenRecht().isAuthorized());
		dto.setOnderzoeken(dto.getOnderzoekenRecht().isAuthorized());
		dto.setSignaleren(dto.getSignalerenRecht().isAuthorized());
		dto.setKwaliteitsopname(dto.getKwaliteitsopnameRecht().isAuthorized());
		dto.setConnectiestatus(dto.getConnectiestatusRecht().isAuthorized());

		return dto;
	}

	private SERechtDto getSeRechtDto(InstellingGebruiker instellingGebruiker, SecurityManager securityManager, PrincipalCollection principalCollection, Recht recht)
	{
		SERechtDto seRechtDto = new SERechtDto();
		Actie benodigdeActie = getBenodigdeActie(recht);
		seRechtDto.setAuthorized(securityManager.isPermitted(principalCollection, buildConstraint(recht, benodigdeActie)));
		setEinddatumOpBasisVanRollen(seRechtDto, instellingGebruiker, recht, benodigdeActie);

		LocalDate gebruikerActiefTotEnMet = DateUtil.toLocalDate(instellingGebruiker.getMedewerker().getActiefTotEnMet());
		if (gebruikerActiefTotEnMet != null && (seRechtDto.getEindDatum() == null || gebruikerActiefTotEnMet.compareTo(seRechtDto.getEindDatum()) < 0))
		{
			seRechtDto.setEindDatum(gebruikerActiefTotEnMet);
		}
		return seRechtDto;
	}

	private void setEinddatumOpBasisVanRollen(SERechtDto seRechtDto, InstellingGebruiker instellingGebruiker, Recht recht, Actie benodigdeActie)
	{
		LocalDate maxEindDatum = null;

		for (InstellingGebruikerRol instellingGebruikerRol : instellingGebruiker.getRollen())
		{
			if (instellingGebruikerRol.isRolActief())
			{
				for (Permissie permissie : instellingGebruikerRol.getRol().getPermissies())
				{
					if (permissie.getRecht().equals(recht) && ArrayUtils.contains(permissie.getRecht().getActie(), benodigdeActie))
					{
						LocalDate eindDatum = DateUtil.toLocalDate(instellingGebruikerRol.getEindDatum());
						if (eindDatum == null)
						{
							seRechtDto.setEindDatum(null);
							return;
						}
						else
						{
							if (maxEindDatum == null || eindDatum.compareTo(maxEindDatum) > 0)
							{
								maxEindDatum = eindDatum;
							}
						}
					}
				}
			}
		}
		seRechtDto.setEindDatum(maxEindDatum);
	}

	private Actie getBenodigdeActie(Recht recht)
	{
		switch (recht)
		{
		case GEBRUIKER_SCREENING_MAMMA_SE_KWALITEITSOPNAME:
			return Actie.TOEVOEGEN;
		case GEBRUIKER_SCREENING_MAMMA_SE_CONNECTIESTATUS_INZIEN:
			return Actie.INZIEN;
		default:
			return Actie.VERWIJDEREN;
		}
	}

	private boolean checkPermission(Long accountId, Recht recht, Actie actie)
	{
		return ThreadContext.getSecurityManager().isPermitted(createSimplePrincipalCollection(accountId), buildConstraint(recht, actie));
	}

	private Constraint buildConstraint(Recht recht, Actie actie)
	{
		Constraint constraintToCheck = new Constraint();
		constraintToCheck.setRecht(recht);
		constraintToCheck.setActie(actie);
		constraintToCheck.setCheckScope(false);
		return constraintToCheck;
	}

	private SimplePrincipalCollection createSimplePrincipalCollection(Long accountId)
	{
		return new SimplePrincipalCollection(new ScreenitPrincipal(InstellingGebruiker.class, accountId, false), seRealm.getName());
	}
}
