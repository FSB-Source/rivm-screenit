package nl.rivm.screenit.main.web.gebruiker.screening.mamma.kwaliteitscontrole.visitatie;

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

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.main.service.mamma.MammaBeoordelingsEenheidService;
import nl.rivm.screenit.main.service.mamma.MammaKwaliteitscontroleService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.component.panels.UploadDocumentFormComponentPanel;
import nl.rivm.screenit.main.web.component.validator.AantalBestandenUploadenValidator;
import nl.rivm.screenit.main.web.component.validator.FileValidator;
import nl.rivm.screenit.model.BeoordelingsEenheid;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.FileType;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.helper.HibernateMagicNumber;
import nl.rivm.screenit.model.mamma.MammaVisitatie;
import nl.rivm.screenit.model.mamma.enums.MammaVisitatieOnderdeel;
import nl.rivm.screenit.model.mamma.enums.MammaVisitatieStatus;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.markup.form.validation.UniqueFieldValidator;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.commons.collections.CollectionUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxButton;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.EnumLabel;
import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.upload.FileUpload;
import org.apache.wicket.markup.html.form.upload.FileUploadField;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.markup.repeater.RepeatingView;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.model.util.ListModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public abstract class MammaVisitatieEditPopupPanel extends GenericPanel<MammaVisitatie>
{
	private static final Logger LOG = LoggerFactory.getLogger(MammaVisitatieEditPopupPanel.class);

	@SpringBean
	private MammaKwaliteitscontroleService kwaliteitscontroleService;

	@SpringBean
	private MammaBeoordelingsEenheidService beoordelingsEenheidService;

	@SpringBean
	private HibernateService hibernateService;

	private Map<MammaVisitatieOnderdeel, IModel<List<FileUpload>>> filesUploaded = new HashMap<>();

	public MammaVisitatieEditPopupPanel(String id, IModel<MammaVisitatie> model)
	{
		super(id, model);

		Form<MammaVisitatie> form = new ScreenitForm<>("form", model);
		MammaVisitatie visitatie = model.getObject();
		form.setEnabled(ScreenitSession.get().checkPermission(Recht.GEBRUIKER_VISITATIE, Actie.AANPASSEN) && visitatie.getGestartOp() == null);
		add(form);

		List<BeoordelingsEenheid> beoordelingsEenheden = beoordelingsEenheidService.getBeoordelingsEenheden(ScreenitSession.get().getInstelling());
		ComponentHelper.addTextField(form, "omschrijving", true, HibernateMagicNumber.L256, String.class, false)
			.add(new UniqueFieldValidator<>(MammaVisitatie.class, getModelObject().getId(), "omschrijving", hibernateService));
		ScreenitDropdown<BeoordelingsEenheid> beDropdown = new ScreenitDropdown<>("beoordelingsEenheid", ModelUtil.listRModel(beoordelingsEenheden, false),
			new ChoiceRenderer<>("naam"));
		beDropdown.setRequired(true);
		form.add(beDropdown);

		boolean statusEnabled = (MammaVisitatieStatus.INGEPLAND.equals(visitatie.getStatus()) || MammaVisitatieStatus.VRIJGEGEVEN.equals(visitatie.getStatus()))
			&& visitatie.getGestartOp() == null;
		List<MammaVisitatieStatus> statussen = new ArrayList<>(Arrays.asList(MammaVisitatieStatus.values()));
		if (statusEnabled)
		{
			statussen.remove(MammaVisitatieStatus.UITGEVOERD);
		}
		ComponentHelper.addDropDownChoice(form, "status", true, statussen, false).setEnabled(statusEnabled);

		RepeatingView onderdelen = new RepeatingView("onderdelen");
		form.add(onderdelen);
		onderdelen.setVisible(visitatie.getGestartOp() == null);
		for (MammaVisitatieOnderdeel onderdeel : MammaVisitatieOnderdeel.values())
		{
			WebMarkupContainer onderdeelContainer = new WebMarkupContainer(onderdelen.newChildId());
			onderdeelContainer.add(new EnumLabel<>("onderdeel", onderdeel));

			filesUploaded.put(onderdeel, new ListModel<>());
			FileUploadField clientenlijst = new FileUploadField("clientenlijst", filesUploaded.get(onderdeel));
			clientenlijst.add(new AantalBestandenUploadenValidator(1));
			clientenlijst.add(new FileValidator(FileType.CSV));
			onderdeelContainer.add(clientenlijst);
			onderdelen.add(onderdeelContainer);
		}
		UploadDocumentFormComponentPanel rapportageBijlagePanel = new UploadDocumentFormComponentPanel("rapportageBijlage",
			new PropertyModel<>(getModel(), "rapportageBijlage"), new FileValidator(FileType.EXCEL_NIEUW));
		UploadDocumentFormComponentPanel vragenlijstBijlagePanel = new UploadDocumentFormComponentPanel("vragenlijstBijlage",
			new PropertyModel<>(getModel(), "vragenlijstBijlage"), new FileValidator(FileType.EXCEL_NIEUW));

		form.add(rapportageBijlagePanel);
		form.add(vragenlijstBijlagePanel);

		IndicatingAjaxButton opslaan = new IndicatingAjaxButton("opslaan")
		{
			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				super.onSubmit(target);

				MammaVisitatie visitatie = MammaVisitatieEditPopupPanel.this.getModelObject();
				File file = null;
				try
				{
					Map<MammaVisitatieOnderdeel, File> fileMap = new HashMap<>();
					Map<File, String> fileNameMap = new HashMap<>();
					for (MammaVisitatieOnderdeel onderdelen : MammaVisitatieOnderdeel.values())
					{
						List<FileUpload> files = filesUploaded.get(onderdelen).getObject();
						if (CollectionUtils.isNotEmpty(files))
						{
							FileUpload upload = files.get(0);
							file = upload.writeToTempFile();
							fileMap.put(onderdelen, file);
							fileNameMap.put(file, upload.getClientFileName());
						}
					}
					UploadDocument rapportageBijlageDocument = rapportageBijlagePanel.getUploadDocumentFromSelectedFile();
					UploadDocument vragenlijstBijlageDocument = vragenlijstBijlagePanel.getUploadDocumentFromSelectedFile();
					List<String> meldingen = new ArrayList<>(
						kwaliteitscontroleService.saveOrUpdateVisitatie(visitatie, fileMap, fileNameMap, rapportageBijlageDocument, vragenlijstBijlageDocument));
					if (CollectionUtils.isNotEmpty(meldingen))
					{
						meldingen.forEach(this::warn);
					}
					else
					{
						info(getString("message.gegevensopgeslagen"));
					}
				}
				catch (Exception e)
				{
					String errorMessage = "Er is een fout opgetreden: " + e.getMessage();
					LOG.error(errorMessage, e);
					error(errorMessage);
				}
				finally
				{
					if (file != null && file.exists())
					{
						file.delete();
					}
				}
				if (!hasErrorMessage())
				{
					MammaVisitatieEditPopupPanel.this.onOpslaanSuccesvol(target);
				}
			}
		};
		form.add(opslaan);
	}

	protected abstract void onOpslaanSuccesvol(AjaxRequestTarget target);

}
