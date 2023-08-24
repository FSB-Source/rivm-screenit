package nl.rivm.screenit.main.web.gebruiker.screening.colon.huisartsimport;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.Arrays;
import java.util.List;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.main.service.ZorgmailImportPoolExecuterService;
import nl.rivm.screenit.main.service.cervix.CervixBulkHuisartsenService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.validator.AantalBestandenUploadenValidator;
import nl.rivm.screenit.main.web.component.validator.FileValidator;
import nl.rivm.screenit.main.web.gebruiker.screening.colon.ColonScreeningBasePage;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.cervix.CervixBulkUpload;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.FileType;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.AutorisatieService;
import nl.rivm.screenit.service.BerichtToBatchService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.form.CheckBox;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.upload.FileUpload;
import org.apache.wicket.markup.html.form.upload.FileUploadField;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.util.ListModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.util.lang.Bytes;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.AANPASSEN,
	constraint = ShiroConstraint.HasPermission,
	recht = Recht.GEBRUIKER_BEHEER_HUISARTSIMPORT,
	bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX })
public class HuisartsImportBeheerPage extends ColonScreeningBasePage
{
	@SpringBean
	private ZorgmailImportPoolExecuterService importExecService;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private LogService logService;

	@SpringBean
	private ICurrentDateSupplier currentDateSupplier;

	@SpringBean
	private AutorisatieService autorisatieService;

	@SpringBean
	private UploadDocumentService uploadDocumentService;

	@SpringBean
	private BerichtToBatchService berichtToBatchService;

	@SpringBean
	private CervixBulkHuisartsenService cervixBulkHuisartsenService;

	@SpringBean
	private SimplePreferenceService preferenceService;

	private CheckBox ediAdresOverschrijven;

	private final IModel<List<FileUpload>> fileUploadModel = new ListModel<>();

	private final IModel<List<FileUpload>> cervixBulkUploadModel = new ListModel<>();

	public HuisartsImportBeheerPage()
	{
		add(getHuisartsImporterenContainer());
		add(getBulkContainer());
	}

	private WebMarkupContainer getHuisartsImporterenContainer()
	{
		WebMarkupContainer container = new WebMarkupContainer("colonHuisartsImporteren");
		Form<Void> uploadForm = new Form<Void>("uploadForm");
		uploadForm.setMultiPart(true);
		FileUploadField uploadveld = new FileUploadField("fileInput", fileUploadModel);
		uploadveld.add(new FileValidator(FileType.CSV));
		uploadveld.add(new AantalBestandenUploadenValidator(1));
		uploadForm.add(uploadveld);
		uploadForm.setMaxSize(Bytes.megabytes(40));
		ediAdresOverschrijven = new CheckBox("ediAdresOverschrijven", Model.of(Boolean.TRUE));
		uploadForm.add(ediAdresOverschrijven);
		uploadForm.add(new IndicatingAjaxSubmitLink("importeer")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				if (CollectionUtils.isNotEmpty(fileUploadModel.getObject()))
				{
					try
					{
						String ediParameter = preferenceService.getString(PreferenceKey.EDIFACTADRES.name());

						if (StringUtils.isBlank(ediParameter) && Boolean.TRUE.equals(ediAdresOverschrijven.getModelObject()))
						{
							error(getString("error.huisartsimport.mislukt.ediadres"));
						}
						else
						{
							FileUpload excelfile = fileUploadModel.getObject().get(0);

							if (!StringUtils.endsWith(excelfile.getClientFileName(), ".csv"))
							{
								error(getString("error.huisartsimport.mislukt.herkenning"));
							}
							else
							{
								InstellingGebruiker ingelogdeMedewerker = ScreenitSession.get().getLoggedInInstellingGebruiker();
								importExecService.startImport(excelfile.writeToTempFile(), ediAdresOverschrijven.getModelObject());

								logService.logGebeurtenis(LogGebeurtenis.HUISARTS_IMPORT_GESTART, ingelogdeMedewerker,
									"Handmatig importeren van Huisartsen Adresboek is gestart. EDI-adres overschrijven: " + ediAdresOverschrijven.getModelObject(),
									Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.MAMMA);
								info(getString("info.huisartsimport.gestart"));
							}
						}
					}
					catch (Exception e)
					{
						error(getString("error.huisartsimport.mislukt.inputstream"));
					}
				}
				else
				{
					error(getString("error.huisartsimport.mislukt.bestand"));
				}
			}
		});
		container.add(uploadForm);
		container.setVisible(magContainerZichtbaarZijnVoor(Recht.GEBRUIKER_BEHEER_HUISARTSIMPORT, Bevolkingsonderzoek.COLON));
		return container;
	}

	private WebMarkupContainer getBulkContainer()
	{
		WebMarkupContainer container = new WebMarkupContainer("cervixBulkContainer");
		Form<Void> cervixUploadBulkForm = new Form<Void>("cervixBulkUploadFrom");
		FileUploadField cervixBulkUploadVeld = new FileUploadField("bulkFile", cervixBulkUploadModel);
		cervixBulkUploadVeld
			.add(new FileValidator(FileType.CSV));
		cervixBulkUploadVeld.add(new AantalBestandenUploadenValidator(1));
		cervixUploadBulkForm.add(cervixBulkUploadVeld);
		cervixUploadBulkForm.setMaxSize(Bytes.megabytes(40));
		cervixUploadBulkForm.add(new AjaxSubmitLink("bulkSubmit")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				if (CollectionUtils.isNotEmpty(cervixBulkUploadModel.getObject()))
				{
					try
					{
						FileUpload excelfile = cervixBulkUploadModel.getObject().get(0);

						if (!StringUtils.endsWith(excelfile.getClientFileName(), ".csv"))
						{
							error(getString("error.huisartsimport.mislukt.herkenning"));
						}

						InstellingGebruiker ingelogdeMedewerker = ScreenitSession.get().getLoggedInInstellingGebruiker();
						CervixBulkUpload upload = cervixBulkHuisartsenService.saveExcelBestand(excelfile.writeToTempFile(), excelfile.getContentType(),
							excelfile.getClientFileName(), ingelogdeMedewerker);
						info("Bulk huisarts bestand is opgeslagen en zal worden verwerkt.");
						cervixBulkHuisartsenService.verwerkBulkHuisartsen(upload);
					}
					catch (Exception e)
					{
						error(getString("error.huisartsimport.mislukt.inputstream"));
					}
				}
				else
				{
					error(getString("error.huisartsimport.mislukt.bestand"));
				}
			}
		});
		container.add(cervixUploadBulkForm);
		container.setVisible(magContainerZichtbaarZijnVoor(Recht.GEBRUIKER_BEHEER_HUISARTSIMPORT, Bevolkingsonderzoek.CERVIX));
		return container;
	}

	private boolean magContainerZichtbaarZijnVoor(Recht controleRecht, Bevolkingsonderzoek onderzoek)
	{
		List<Bevolkingsonderzoek> onderzoeken = ScreenitSession.get().getOnderzoeken();
		if (onderzoeken.contains(onderzoek))
		{
			List<Recht> rechten = autorisatieService.getRechtWithBevolkingsonderzoek(Arrays.asList(onderzoek));
			for (Recht recht : rechten)
			{
				if (controleRecht.equals(recht))
				{
					return true;
				}
			}
		}
		return false;
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(fileUploadModel);
		ModelUtil.nullSafeDetach(cervixBulkUploadModel);
	}
}
