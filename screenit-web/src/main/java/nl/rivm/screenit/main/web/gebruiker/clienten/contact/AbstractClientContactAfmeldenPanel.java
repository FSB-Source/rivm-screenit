package nl.rivm.screenit.main.web.gebruiker.clienten.contact;

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
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.service.ClientContactService;
import nl.rivm.screenit.model.enums.ExtraOpslaanKey;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.component.validator.FileValidator;
import nl.rivm.screenit.model.AanvraagBriefStatus;
import nl.rivm.screenit.model.Afmelding;
import nl.rivm.screenit.model.AfmeldingType;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContactActie;
import nl.rivm.screenit.model.ClientContactManier;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.enums.FileType;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.form.DropDownChoice;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.RadioChoice;
import org.apache.wicket.markup.html.form.upload.FileUpload;
import org.apache.wicket.markup.html.form.upload.FileUploadField;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.util.ListModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public abstract class AbstractClientContactAfmeldenPanel<A extends Afmelding, E extends Enum<E>> extends AbstractClientContactActiePanel<ClientContactActie>
{
	private static final Logger LOG = LoggerFactory.getLogger(AbstractClientContactAfmeldenPanel.class);

	@SpringBean
	protected ClientContactService clientContactService;

	protected IModel<A> afmeldingModel;

	protected IModel<Client> clientModel;

	private IModel<UploadDocument> afmeldHandtekeningUploadDocumentModel;

	private IModel<List<FileUpload>> files = new ListModel<>();

	private WebMarkupContainer manierContainer;

	private WebMarkupContainer bestandSelecterenContainer;

	protected WebMarkupContainer redenenContainer;

	public AbstractClientContactAfmeldenPanel(String id, IModel<ClientContactActie> clientContactActieModel, IModel<Client> clientModel, List<Object> extraPanelParams)
	{
		super(id, clientContactActieModel);
		this.clientModel = clientModel;

		afmeldingModel = getAfmeldingModel(extraPanelParams);
		WebMarkupContainer container = new WebMarkupContainer("container", afmeldingModel);
		add(container);

		List<AfmeldingType> availableAfmeldopties = getAvailableAfmeldopties(clientModel);
		DropDownChoice<AfmeldingType> type = new ScreenitDropdown<>("type", availableAfmeldopties, new EnumChoiceRenderer<>(this));
		type.setRequired(true);
		type.add(new AjaxFormComponentUpdatingBehavior("change")
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				A afmelding = AbstractClientContactAfmeldenPanel.this.afmeldingModel.getObject();

				afmelding.setManier(null);
				manierContainer.setVisible(AfmeldingType.DEFINITIEF.equals(afmelding.getType()));
				target.add(manierContainer);

				bestandSelecterenContainer.setVisible(false);
				target.add(bestandSelecterenContainer);

				setRedenenContainerVisible(target, afmelding);
			}
		});
		container.add(type);

		DropDownChoice<ClientContactManier> manier = new ScreenitDropdown<>("manier", new ListModel<>(Arrays.asList(ClientContactManier.values())), new EnumChoiceRenderer<>(this));
		manier.setOutputMarkupPlaceholderTag(true);
		manier.setRequired(true);
		manier.add(new AjaxFormComponentUpdatingBehavior("change")
		{
			private static final long serialVersionUID = 1L;

			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				A afmelding = AbstractClientContactAfmeldenPanel.this.afmeldingModel.getObject();
				bestandSelecterenContainer.setVisible(ClientContactManier.DIRECT.equals(afmelding.getManier()));
				target.add(bestandSelecterenContainer);

				setRedenenContainerVisible(target, afmelding);
			}
		});

		manierContainer = new WebMarkupContainer("manierContainer");
		manierContainer.setOutputMarkupPlaceholderTag(true);
		manierContainer.setVisible(false);
		manierContainer.add(manier);
		container.add(manierContainer);

		FileUploadField uploadField = new FileUploadField("bestandSelecteren", files);
		uploadField.add(new FileValidator(FileType.PDF));
		uploadField.setRequired(true);

		bestandSelecterenContainer = new WebMarkupContainer("bestandSelecterenContainer");
		bestandSelecterenContainer.setOutputMarkupPlaceholderTag(true);
		bestandSelecterenContainer.setVisible(false);
		bestandSelecterenContainer.add(uploadField);
		container.add(bestandSelecterenContainer);

		redenenContainer = getRedenenContainer();
		container.add(redenenContainer);
	}

	private WebMarkupContainer getRedenenContainer()
	{
		RadioChoice<E> reden = new RadioChoice<E>("reden", getRedenenModel(), new EnumChoiceRenderer<E>(this));
		reden.setPrefix("<label class=\"radio\">");
		reden.setSuffix("</label>");
		reden.setRequired(true);

		WebMarkupContainer redenContainer = new WebMarkupContainer("redenContainer");
		redenContainer.setVisible(false);
		redenContainer.setOutputMarkupPlaceholderTag(true);
		redenContainer.add(reden);

		return redenContainer;
	}

	protected abstract IModel<List<E>> getRedenenModel();

	@Override
	public Map<ExtraOpslaanKey, Object> getOpslaanObjecten()
	{
		Map<ExtraOpslaanKey, Object> objecten = new HashMap<>();
		A afmelding = afmeldingModel.getObject();

		if (AfmeldingType.EENMALIG.equals(afmelding.getType()))
		{
			afmelding.setManier(ClientContactManier.DIRECT);
		}
		else if (AfmeldingType.DEFINITIEF.equals(afmelding.getType()) && ClientContactManier.DIRECT.equals(afmelding.getManier()) && afmeldHandtekeningUploadDocumentModel != null)
		{
			afmelding.setAfmeldingStatus(AanvraagBriefStatus.BRIEF);
			UploadDocument document = afmeldHandtekeningUploadDocumentModel.getObject();
			afmelding.setHandtekeningDocumentAfmelding(document);
			objecten.put(ExtraOpslaanKey.AFMELDING_BEVESTIGING_DOCUMENT, document);
		}

		objecten.put(ExtraOpslaanKey.AFMELDING, afmelding);
		return objecten;
	}

	@Override
	public List<String> getOpslaanMeldingen()
	{
		try
		{
			if (files.getObject() != null)
			{
				FileUpload upload = files.getObject().get(0);
				File definitieFile;
				definitieFile = upload.writeToTempFile();
				UploadDocument document = new UploadDocument();
				document.setActief(Boolean.TRUE);
				document.setContentType(upload.getContentType());
				document.setFile(definitieFile);
				document.setNaam(upload.getClientFileName());
				afmeldHandtekeningUploadDocumentModel = ModelUtil.cModel(document);
			}
		}
		catch (Exception e)
		{
			LOG.error("Er gaat iets mis met de filestore", e);
		}
		return super.getOpslaanMeldingen();
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(afmeldingModel);
		ModelUtil.nullSafeDetach(clientModel);
	}

	protected abstract List<AfmeldingType> getAvailableAfmeldopties(IModel<Client> client);

	protected abstract IModel<A> getAfmeldingModel(List<Object> extraPanelParams);

	protected abstract void setRedenenContainerVisible(AjaxRequestTarget target, A afmelding);
}
