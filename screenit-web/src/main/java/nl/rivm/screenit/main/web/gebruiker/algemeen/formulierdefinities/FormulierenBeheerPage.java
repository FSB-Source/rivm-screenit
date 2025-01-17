package nl.rivm.screenit.main.web.gebruiker.algemeen.formulierdefinities;

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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import nl.rivm.screenit.main.service.FormulierService;
import nl.rivm.screenit.main.web.component.SimpleStringResourceModel;
import nl.rivm.screenit.main.web.component.validator.FileValidator;
import nl.rivm.screenit.main.web.gebruiker.algemeen.AlgemeenPage;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerBasePage;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerHoofdMenuItem;
import nl.rivm.screenit.main.web.gebruiker.gedeeld.formulieren.FormulierRenderPreviewPage;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.FileType;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.formulieren.ScreenitFormulierInstantie;
import nl.rivm.screenit.model.formulieren.TypeFormulier;
import nl.rivm.screenit.util.EnumStringUtil;
import nl.topicuszorg.wicket.hibernate.SimpleListHibernateModel;

import org.apache.wicket.markup.html.form.DropDownChoice;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.SubmitLink;
import org.apache.wicket.markup.html.form.upload.FileUpload;
import org.apache.wicket.markup.html.form.upload.FileUploadField;
import org.apache.wicket.markup.html.link.Link;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.util.ListModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.wicketstuff.datetime.markup.html.basic.DateLabel;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.AANPASSEN,
	constraint = ShiroConstraint.HasPermission,
	recht = Recht.GEBRUIKER_FORMULIER_BEHEER,
	bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
public class FormulierenBeheerPage extends AlgemeenPage
{
	private static final Logger LOG = LoggerFactory.getLogger(FormulierenBeheerPage.class);

	@SpringBean
	private FormulierService formulierService;

	public FormulierenBeheerPage()
	{
		add(new UploadForm("uploadForm"));
		add(maakLijst());
	}

	private class UploadForm extends Form<Void>
	{

		private final IModel<List<FileUpload>> fileUploads = new ListModel<>();

		private final IModel<TypeFormulier> typeFormulier = new Model<>();

		public UploadForm(String id)
		{
			super(id);

			List<TypeFormulier> choices = new ArrayList<>();
			choices.add(TypeFormulier.MDL);
			choices.add(TypeFormulier.PALGA);
			choices.add(TypeFormulier.CYTOLOGIE);
			choices.add(TypeFormulier.MAMMA_PA_FOLLOW_UP);
			add(new DropDownChoice<>("typeFormulier", typeFormulier, choices, new EnumChoiceRenderer<TypeFormulier>()));
			add(new FileUploadField("upload", fileUploads).add(new FileValidator(FileType.EXCEL_NIEUW)));

			add(new SubmitLink("opslaan")
			{
				@Override
				public void onSubmit()
				{
					super.onSubmit();
					try
					{
						TypeFormulier formulierType = typeFormulier.getObject();
						if (formulierType == null)
						{
							error(getString("error.geen.formuliertype"));
						}
						else
						{
							if (fileUploads.getObject() != null)
							{
								formulierService.importFormulier(formulierType, fileUploads.getObject().get(0).writeToTempFile(), formulierType.name(), true);
							}
							else
							{
								error(getString("error.geen.bestand"));
							}
						}
						FormulierenBeheerPage.this.addOrReplace(maakLijst());
					}
					catch (Exception e)
					{
						LOG.error(e.getMessage(), e);
						error(e.getMessage());
					}
				}
			});
		}
	}

	private ListView<ScreenitFormulierInstantie> maakLijst()
	{

		List<ScreenitFormulierInstantie> formulierInstanties = new ArrayList<>();
		formulierInstanties.add(formulierService.getFormulierInstantie(TypeFormulier.MDL));
		formulierInstanties.add(formulierService.getFormulierInstantie(TypeFormulier.PALGA));
		formulierInstanties.add(formulierService.getFormulierInstantie(TypeFormulier.CYTOLOGIE));
		formulierInstanties.add(formulierService.getFormulierInstantie(TypeFormulier.MAMMA_PA_FOLLOW_UP));
		formulierInstanties.removeAll(Collections.singleton(null));
		return new ListView<>("formulieren", new SimpleListHibernateModel<>(formulierInstanties))
		{
			@Override
			protected void populateItem(ListItem<ScreenitFormulierInstantie> item)
			{
				Link<ScreenitFormulierInstantie> renderLink = new Link<>("renderLink", item.getModel())
				{
					@Override
					public void onClick()
					{
						setResponsePage(new FormulierRenderPreviewPage(getModel())
						{
							@Override
							protected Class<? extends GebruikerBasePage> getActiveSubMenuClass()
							{
								return FormulierenBeheerPage.class;
							}

							@Override
							protected GebruikerHoofdMenuItem getActieveMenuItem()
							{
								return GebruikerHoofdMenuItem.ALGEMEEN;
							}

							@Override
							protected void terug()
							{
								setResponsePage(FormulierenBeheerPage.class);
							}
						});
					}

				};
				renderLink.setBody(new SimpleStringResourceModel(EnumStringUtil.getPropertyString(item.getModelObject().getTypeFormulier())));
				item.add(renderLink);
				item.add(DateLabel.forDatePattern("createDatum", Model.of(item.getModelObject().getCreatieDatum()), "dd-MM-yyyy"));
			}
		};
	}

}
