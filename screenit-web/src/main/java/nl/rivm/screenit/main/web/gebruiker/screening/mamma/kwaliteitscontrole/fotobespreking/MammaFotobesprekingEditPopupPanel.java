package nl.rivm.screenit.main.web.gebruiker.screening.mamma.kwaliteitscontrole.fotobespreking;

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

import java.io.File;
import java.util.Arrays;
import java.util.List;

import nl.rivm.screenit.main.service.mamma.MammaBeoordelingsEenheidService;
import nl.rivm.screenit.main.service.mamma.MammaKwaliteitscontroleService;
import nl.rivm.screenit.main.service.mamma.MammaScreeningsEenheidService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.component.validator.AantalBestandenUploadenValidator;
import nl.rivm.screenit.main.web.component.validator.FileValidator;
import nl.rivm.screenit.model.BeoordelingsEenheid;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.FileType;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.helper.HibernateMagicNumber;
import nl.rivm.screenit.model.mamma.MammaFotobespreking;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.enums.MammaFotobesprekingType;
import nl.rivm.screenit.model.mamma.enums.MammobridgeRole;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.markup.form.validation.UniqueFieldValidator;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.commons.collections.CollectionUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxButton;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.upload.FileUpload;
import org.apache.wicket.markup.html.form.upload.FileUploadField;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.util.ListModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public abstract class MammaFotobesprekingEditPopupPanel extends GenericPanel<MammaFotobespreking>
{
	private static final Logger LOG = LoggerFactory.getLogger(MammaFotobesprekingEditPopupPanel.class);

	@SpringBean
	private MammaKwaliteitscontroleService kwaliteitscontroleService;

	@SpringBean
	private MammaScreeningsEenheidService screeningsEenheidService;

	@SpringBean
	private MammaBeoordelingsEenheidService beoordelingsEenheidService;

	@SpringBean
	private HibernateService hibernateService;

	private IModel<List<FileUpload>> filesUploaded = new ListModel<>();

	public MammaFotobesprekingEditPopupPanel(String id, IModel<MammaFotobespreking> model)
	{
		super(id, model);

		Form<MammaFotobespreking> form = new ScreenitForm<>("form", model);
		form.setEnabled(ScreenitSession.get().checkPermission(Recht.GEBRUIKER_FOTOBESPREKING, Actie.AANPASSEN) && model.getObject().getGestartOp() == null);
		add(form);

		List<BeoordelingsEenheid> beoordelingsEenheden = beoordelingsEenheidService.getBeoordelingsEenheden(ScreenitSession.get().getInstelling());
		List<MammaScreeningsEenheid> screeningsEenheden = screeningsEenheidService.getActieveScreeningsEenhedenVoorBeoordelingsEenheden(beoordelingsEenheden);
		ComponentHelper.addTextField(form, "omschrijving", true, HibernateMagicNumber.L256, String.class, false)
			.add(new UniqueFieldValidator<>(MammaFotobespreking.class, getModelObject().getId(), "omschrijving", hibernateService));
		WebMarkupContainer beContainer = new WebMarkupContainer("beoordelingsEenheidContainer");
		form.add(beContainer);
		ScreenitDropdown<BeoordelingsEenheid> beDropdown = new ScreenitDropdown<>("beoordelingsEenheid", ModelUtil.listRModel(beoordelingsEenheden, false),
			new ChoiceRenderer<>("naam"));
		beDropdown.setNullValid(true);
		beContainer.setOutputMarkupId(true);
		beContainer.setOutputMarkupPlaceholderTag(true);
		beContainer.add(beDropdown);
		WebMarkupContainer seContainer = new WebMarkupContainer("screeningsEenheidContainer");
		form.add(seContainer);
		ScreenitDropdown<MammaScreeningsEenheid> seDropdown = new ScreenitDropdown<>("screeningsEenheid", ModelUtil.listRModel(screeningsEenheden, false),
			new ChoiceRenderer<>("naam"));
		seDropdown.setNullValid(true);
		seContainer.setOutputMarkupId(true);
		seContainer.setOutputMarkupPlaceholderTag(true);
		seContainer.add(seDropdown);
		showBEEnOfSE(beContainer, seContainer, model.getObject());
		ComponentHelper.addDropDownChoice(form, "type", true, Arrays.asList(MammaFotobesprekingType.values()), false).add(new AjaxFormComponentUpdatingBehavior("change")
		{

			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				MammaFotobespreking fotobespreking = form.getModelObject();
				showBEEnOfSE(beContainer, seContainer, fotobespreking);
				target.add(beContainer, seContainer);
			}

		});
		ComponentHelper.addDropDownChoice(form, "role", true, Arrays.asList(MammobridgeRole.values()), false);
		FileUploadField clientenlijst = new FileUploadField("clientenlijst", filesUploaded);
		clientenlijst.add(new AantalBestandenUploadenValidator(1));
		clientenlijst.add(new FileValidator(FileType.CSV));
		clientenlijst.setVisible(model.getObject().getGestartOp() == null);
		form.add(clientenlijst);

		IndicatingAjaxButton opslaan = new IndicatingAjaxButton("opslaan")
		{
			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				super.onSubmit(target);

				MammaFotobespreking fotobespreking = MammaFotobesprekingEditPopupPanel.this.getModelObject();
				if (fotobespreking.getBeoordelingsEenheid() == null && fotobespreking.getScreeningsEenheid() == null)
				{
					error(getString("niets.gekozen"));
					return;
				}
				else if (fotobespreking.getBeoordelingsEenheid() != null && fotobespreking.getScreeningsEenheid() != null)
				{
					error(getString("beide.gekozen"));
					return;
				}
				File file = null;
				try
				{
					List<FileUpload> files = filesUploaded.getObject();
					if (CollectionUtils.isNotEmpty(files))
					{
						FileUpload upload = files.get(0);
						file = upload.writeToTempFile();
					}
					List<String> meldingen = kwaliteitscontroleService.saveOrUpdateFotobespreking(fotobespreking, file);
					if (CollectionUtils.isNotEmpty(meldingen))
					{
						meldingen.forEach(m -> warn(m));
						warn(getString("message.gegevensopgeslagen.met.meldingen"));
					}
					else
					{
						info(getString("message.gegevensopgeslagen"));
					}
				}
				catch (Exception e)
				{
					LOG.error("Er is een fout opgetreden: ", e);
					error(getString("bestand.upload.fout"));
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
					MammaFotobesprekingEditPopupPanel.this.onOpslaanSuccesvol(target);
				}
			}
		};
		form.add(opslaan);
	}

	private void showBEEnOfSE(WebMarkupContainer beContainer, WebMarkupContainer seContainer, MammaFotobespreking fotobespreking)
	{
		beContainer.setVisible(false);
		seContainer.setVisible(false);
		if (fotobespreking.getType() != null)
		{
			switch (fotobespreking.getType())
			{
			case INDIVIDUELE_TOETSING_INSTELTECHNIEK:
			case TEAM_TOETSING_INSTELTECHNIEK:
				fotobespreking.setBeoordelingsEenheid(null);
				seContainer.setVisible(true);
				break;
			case MAATSCHAP:
				fotobespreking.setScreeningsEenheid(null);
				beContainer.setVisible(true);
				break;
			case RADIOLOOG_MMBER:
				beContainer.setVisible(true);
				seContainer.setVisible(true);
				break;
			default:
				break;
			}
		}
	}

	protected abstract void onOpslaanSuccesvol(AjaxRequestTarget target);

}
