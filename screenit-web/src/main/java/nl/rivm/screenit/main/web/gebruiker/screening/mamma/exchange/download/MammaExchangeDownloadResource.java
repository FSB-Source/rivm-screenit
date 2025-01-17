package nl.rivm.screenit.main.web.gebruiker.screening.mamma.exchange.download;

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

import java.io.FileInputStream;
import java.io.IOException;
import java.time.Duration;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.main.service.mamma.MammaUitwisselportaalService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.UploadDocumentService;

import org.apache.wicket.Application;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.request.mapper.parameter.PageParameters;
import org.apache.wicket.request.resource.AbstractResource;
import org.apache.wicket.request.resource.ContentDisposition;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.jetbrains.annotations.NotNull;
import org.springframework.http.HttpStatus;
import org.wicketstuff.shiro.ShiroConstraint;

@Slf4j
@SecurityConstraint(
	constraint = ShiroConstraint.HasPermission,
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA },
	recht = { Recht.GEBRUIKER_MAMMA_EXCHANGE },
	organisatieTypeScopes = { OrganisatieType.SCREENINGSORGANISATIE, OrganisatieType.RADIOLOGIEAFDELING, OrganisatieType.MAMMAPOLI, OrganisatieType.ZORGINSTELLING,
		OrganisatieType.RIVM },
	checkScope = true) 
public class MammaExchangeDownloadResource extends AbstractResource
{
	@SpringBean
	private UploadDocumentService uploadDocumentService;

	@SpringBean
	private MammaUitwisselportaalService uitwisselPortaalService;

	public MammaExchangeDownloadResource()
	{
		Injector.get().inject(this);
	}

	@Override
	protected ResourceResponse newResourceResponse(Attributes attributes)
	{
		var parameters = attributes.getParameters();
		var downloadVerzoekId = getDownloadVerzoekId(parameters);
		var downloadVerzoek = uitwisselPortaalService.geldigDownloadVerzoekVoorIngelogdeGebruiker(downloadVerzoekId, ScreenitSession.get().getLoggedInInstellingGebruiker());
		if (downloadVerzoek.isEmpty())
		{
			LOG.warn("Geen toegang voor downloadverzoek '{}'", downloadVerzoekId);
			return new ResourceResponse().setError(HttpStatus.FORBIDDEN.value());
		}

		return maakResourceResponse(downloadVerzoek.get().getZipBestand());
	}

	private long getDownloadVerzoekId(PageParameters parameters)
	{
		try
		{
			return parameters.get("id").toLong();
		}
		catch (Exception e)
		{
			LOG.info("parse:", e);
			return -1;
		}
	}

	@NotNull
	private ResourceResponse maakResourceResponse(UploadDocument zipBestand)
	{
		var fileName = zipBestand.getNaam();
		ResourceResponse resourceResponse = new ResourceResponse();
		resourceResponse.setFileName(fileName);
		resourceResponse.setContentType(Application.get().getMimeType(fileName));
		resourceResponse.setCacheDuration(Duration.ofSeconds(1));
		resourceResponse.setContentDisposition(ContentDisposition.ATTACHMENT);

		var file = uploadDocumentService.load(zipBestand);

		resourceResponse.setWriteCallback(new WriteCallback()
		{
			@Override
			public void writeData(Attributes attributes) throws IOException
			{
				try (var fileInputStream = new FileInputStream(file))
				{
					writeStream(attributes, fileInputStream);
				}
			}
		});

		return resourceResponse;
	}
}
