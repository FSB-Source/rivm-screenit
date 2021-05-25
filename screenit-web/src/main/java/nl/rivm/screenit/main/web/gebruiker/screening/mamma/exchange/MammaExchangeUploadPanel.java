package nl.rivm.screenit.main.web.gebruiker.screening.mamma.exchange;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.main.service.mamma.MammaUploadBeeldenService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.component.form.upload.ScreenitFileUploadField;
import nl.rivm.screenit.main.web.component.form.upload.ScreenitUploadProgressBar;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.validator.FileValidator;
import nl.rivm.screenit.main.web.gebruiker.clienten.ClientPaspoortPanel;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.panel.MammaGeenBeeldenBeschikbaarCellPanel;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.enums.FileType;
import nl.rivm.screenit.model.helper.HibernateMagicNumber;
import nl.rivm.screenit.model.mamma.MammaUploadBeeldenVerzoek;
import nl.rivm.screenit.model.mamma.enums.MammaFollowUpBIRADSWaarde;
import nl.rivm.screenit.service.BerichtToBatchService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.commons.lang.StringUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.TextArea;
import org.apache.wicket.markup.html.form.upload.FileUpload;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.util.ListModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.util.lang.Bytes;
import org.apache.wicket.validation.validator.StringValidator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class MammaExchangeUploadPanel extends GenericPanel<MammaUploadBeeldenVerzoek>
{
	@SpringBean
	private MammaUploadBeeldenService uploadBeeldenService;

	@SpringBean
	private BerichtToBatchService berichtToBatchService;

	@SpringBean
	private SimplePreferenceService preferenceService;

	private IModel<List<FileUpload>> files;

	private BootstrapDialog dialog;

	private final Logger LOG = LoggerFactory.getLogger(MammaExchangeUploadPanel.class);

	public MammaExchangeUploadPanel(String id, IModel<MammaUploadBeeldenVerzoek> uploadBeeldenVerzoekModel)
	{
		super(id, uploadBeeldenVerzoekModel);
		dialog = new BootstrapDialog("dialog");
		add(dialog);

		Form<MammaUploadBeeldenVerzoek> form = new Form<>("form");
		form.add(new ScreenitDropdown<>("conclusieBirads", Arrays.asList(MammaFollowUpBIRADSWaarde.values()), new EnumChoiceRenderer<>()).setNullValid(false).setRequired(true));

		TextArea<String> conclusieEersteUitslagRadiologie = new TextArea<>("conclusieEersteUitslagRadiologie");
		conclusieEersteUitslagRadiologie.add(StringValidator.maximumLength(HibernateMagicNumber.L1024));
		conclusieEersteUitslagRadiologie.setRequired(true);
		form.add(conclusieEersteUitslagRadiologie);

		createPassportContainer(form, ModelUtil.cModel(uploadBeeldenVerzoekModel.getObject().getScreeningRonde().getDossier().getClient()));

		form.add(new AjaxLink<Void>("annuleren")
		{
			@Override
			public void onClick(AjaxRequestTarget ajaxRequestTarget)
			{
				onSubmit(ajaxRequestTarget);
			}
		});

		form.add(new MammaGeenBeeldenBeschikbaarCellPanel("geenBeschikbaar", dialog, "geenBeeldenBeschikbaar", uploadBeeldenVerzoekModel)
		{
			@Override
			protected void onOpslaan(AjaxRequestTarget ajaxRequestTarget, MammaUploadBeeldenVerzoek uploadBeeldenVerzoek)
			{
				uploadBeeldenService.setGeenBeeldenBeschikbaar(uploadBeeldenVerzoek, ScreenitSession.get().getLoggedInInstellingGebruiker());
				onSubmit(ajaxRequestTarget);
			}
		});

		IndicatingAjaxSubmitLink opslaanButton = new IndicatingAjaxSubmitLink("opslaan", form)
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				super.onSubmit(target);

				MammaUploadBeeldenVerzoek uploadBeeldenVerzoek = MammaExchangeUploadPanel.this.getModelObject();
				List<UploadDocument> uploadDocumenten = new ArrayList<>();

				try
				{
					for (FileUpload fileUpload : files.getObject())
					{
						File file = fileUpload.writeToTempFile();
						UploadDocument uploadDocument = new UploadDocument();
						uploadDocument.setFile(file);
						uploadDocument.setNaam(fileUpload.getClientFileName());
						uploadDocument.setContentType(fileUpload.getContentType());
						uploadDocument.setActief(true);
						uploadDocumenten.add(uploadDocument);
					}
				}
				catch (Exception e)
				{
					LOG.error("Er is een technische fout opgetreden", e);
					error(getString("fout.opgetreden"));
					return;
				}

				String errorMelding = uploadBeeldenService.uploadBeelden(uploadBeeldenVerzoek, uploadDocumenten, ScreenitSession.get().getLoggedInInstellingGebruiker());
				berichtToBatchService.queueMammaUploadBeeldenVerzoekBericht();
				if (StringUtils.isNotBlank(errorMelding))
				{
					error(getString(errorMelding));
					return;
				}

				MammaExchangeUploadPanel.this.onSubmit(target);
			}
		};
		opslaanButton.setOutputMarkupId(true);
		form.add(opslaanButton);

		createFileUploadField(form, opslaanButton);

		add(form);
	}

	private void createPassportContainer(Form form, IModel<Client> client)
	{
		WebMarkupContainer passport = new ClientPaspoortPanel("paspoort", client);
		passport.setOutputMarkupId(true);
		passport.setOutputMarkupPlaceholderTag(true);
		form.add(passport);
	}

	private void createFileUploadField(Form form, IndicatingAjaxSubmitLink opslaanButton)
	{
		Integer maxTotalUploadSize = preferenceService.getInteger(PreferenceKey.INTERNAL_MAMMA_UPLOADLIMIET_UPLOADPORTAAL.name());
		form.setMaxSize(Bytes.megabytes(maxTotalUploadSize));

		files = new ListModel<>();

		ScreenitFileUploadField fileUploadField = new ScreenitFileUploadField("fileUpload", files, Bytes.megabytes(maxTotalUploadSize));
		form.add(fileUploadField.setRequired(true).add(new FileValidator(FileType.DICOM)));

		ScreenitUploadProgressBar uploadProgressBar = new ScreenitUploadProgressBar("fileUploadProgress", form, opslaanButton, fileUploadField);
		form.add(uploadProgressBar);
	}

	protected void onSubmit(AjaxRequestTarget target)
	{
	}
}
